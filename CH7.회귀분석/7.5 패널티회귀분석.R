# 릿지회귀분석(L2-norm): 모델의 설명력에 기여하지 못하는 독립변수의 회귀계수 크기를 0에 근접하도록 축소
# 라쏘회귀분석(L1-norm): 모델의 설명력에 기여하지 못하는 독립변수의 회귀계수 크기를 0으로 만듦
# 일래스틱넷회귀분석: L1-norm + L2-norm
library(MASS)
str(Boston)
library(caret)
set.seed(123)
train <- createDataPartition(y=Boston$medv, p=0.7, list = F)
Boston.train <- Boston[train,]
Boston.test <- Boston[-train,]

#install.packages("glmnet")
library(glmnet)

# model.matrix: 모델에 투입할 예측변수의 행렬 생성 및 범주형 데이터를 더미변수로 자동으로 변환
x <- model.matrix(medv ~ ., Boston.train)[, -1] # 첫 번째 열은 절편 열로 모든 값이 1을 가짐. 따라서 첫 번째 열은 제외.
y <- Boston.train$medv

## 릿지 회귀분석 ##
set.seed(123)

# 최적의 람다 구하기(MSE가 최소인 람다)
Boston.cv <- cv.glmnet(x=x, y=y, family="gaussian", alpha=0) # 0:릿지, 1:라쏘
plot(Boston.cv) # 로그에 따른 MSE 변화 추이
Boston.cv$lambda.min # 0.6647797
log(Boston.cv$lambda.min) # -0.4082995

# 최적의 람다를 glmnet에 반영
Boston.gnet <- glmnet(x, y, family = "gaussian", alpha = 0, lambda = Boston.cv$lambda.min)
coef(Boston.gnet) # 회귀계수 출력

# 예측모델 성능 확인
Boston.test.x <- model.matrix(medv ~ ., Boston.test)[, -1]
Boston.pred <- predict(Boston.gnet, newx = Boston.test.x)
postResample(pred = Boston.pred, obs = Boston.test$medv)

## 라소 회귀분석 ##
set.seed(123)
Boston.cv <- cv.glmnet(x=x, y=y, family = "gaussian", alpha = 1)
Boston.cv$lambda.min
log(Boston.cv$lambda.min)
plot(Boston.cv)

Boston.cv$lambda.1se
log(Boston.cv$lambda.1se)

# lambda.1se 을 사용할 경우, 불필요한 변수는 제거됨.
coef(Boston.cv, Boston.cv$lambda.min)
coef(Boston.cv, Boston.cv$lambda.1se)

# lambda.min vs lambda.1se
Boston.gnet1 <- glmnet(x, y, family = "gaussian", alpha = 1, lambda = Boston.cv$lambda.min)
Boston.pred1 <- predict(Boston.gnet1, newx = Boston.test.x)
postResample(pred = Boston.pred1, obs = Boston.test$medv)

Boston.gnet2 <- glmnet(x, y, family = "gaussian", alpha = 1, lambda = Boston.cv$lambda.1se)
Boston.pred2 <- predict(Boston.gnet2, newx = Boston.test.x)
postResample(pred = Boston.pred2, obs = Boston.test$medv) # 모델이 간명하지만 정확도는 떨어짐.

## 엘라스틱넷 회귀분석 ##
# alpha가 0과 1사이 값
library(caret)
set.seed(123)
Boston.cv <- train(form = medv ~ ., data = Boston.train, method = "glmnet",
                   trControl = trainControl(method = "cv", number = 10), # 10-fold 교차검증
                   tuneLength = 10) # 10개 조합 중 가장 적절한 alpha와 lambda를 찾아줌.
Boston.cv$bestTune

Boston.gnet <- glmnet(x, y, family = "gaussian",
                      alpha = Boston.cv$bestTune$alpha,
                      lambda = Boston.cv$bestTune$lambda)
coef(Boston.gnet)
Boston.pred <- predict(Boston.gnet, newx = Boston.test.x)
postResample(pred = Boston.pred, obs = Boston.test$medv)

# 패널티회귀분석 모델 비교
# 릿지/라소 모델도 파라미터 튜닝을 통해 최적의 모델을 생성하고 3개 모델의 성능을 비교

# 1. lambda 범위 설정
library(caret)
lambda <- 10 ^ seq(-5, 5, 100)

# 2. 100개의 lambda에 대한 릿지 회귀분석 교차검증 수행
set.seed(123)
ridge <- train(medv ~ ., Boston.train, method = "glmnet",
               trControl = trainControl(method = "cv", number = 10),
               tuneGrid = expand.grid(alpha = 0, lambda = lambda))  # python의 GridsearchCV와 동일
coef(ridge$finalModel, ridge$bestTune$lambda)
ridge.pred <- predict(ridge, Boston.test)
postResample(pred = ridge.pred, obs = Boston.test$medv)

# 3. 100개의 lambda에 대한 라쏘 회귀분석 교차검증 수행
set.seed(123)
lasso <- train(medv ~ ., Boston.train, method = "glmnet",
               trControl = trainControl(method = "cv", number = 10),
               tuneGrid = expand.grid(alpha = 1, lambda = lambda))
coef(lasso$finalModel, lasso$bestTune$lambda)
lasso.pred <- predict(lasso, Boston.test)
postResample(pred = lasso.pred, obs = Boston.test$medv)

# 4. 100개의 lambda에 대한 엘라스틱넷 회귀분석 교차검증 수행
set.seed(123)
elastic <- train(medv ~ ., data=Boston.train, method = "glmnet",
                 trControl = trainControl(method = "cv", number = 10),
                 tuneLength = 10)
coef(elastic$finalModel, elastic$bestTune$lambda)
elastic.pred <- predict(elastic, Boston.test)
postResample(pred = elastic.pred, obs = Boston.test$medv)

# 5. 성능 비교
models <- list(ridge = ridge, lasso = lasso, elastic = elastic)
summary(resamples(models), metric = "RMSE") # RMSE에 따른 차이가 크지 않음.
summary(diff(resamples(models), metric = "RMSE")) # 대각선 아래: p-value.

# 결론: 릿지/라쏘/엘라스틱넷 값이 통계적으로 유의미한 차이가 없다.
