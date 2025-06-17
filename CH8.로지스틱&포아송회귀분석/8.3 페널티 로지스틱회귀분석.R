## 라쏘 회귀분석 ##
#install.packages("mlbench")
library(mlbench)
data("PimaIndiansDiabetes2")
str(PimaIndiansDiabetes2)

# 결측치 제거
PimaIndiansDiabetes3 <- na.omit(PimaIndiansDiabetes2)

# 데이터 분할(7:3)
library(caret)
set.seed(123)
train <- createDataPartition(y = PimaIndiansDiabetes3$diabetes, p = 0.7, list = F)
diabete.train <- PimaIndiansDiabetes3[train, ]
diabete.test <- PimaIndiansDiabetes3[-train, ]

x <- model.matrix(diabetes ~ ., data = diabete.train)[, -1] # 범주형 변수 더미변수로 자동 변환
y <- ifelse(diabete.train$diabetes == "pos", 1, 0)

# 최적의 람다 산출
library(glmnet)
set.seed(123)
diabete.cv <- cv.glmnet(x = x, y = y, family = "binomial", alpha = 1)
diabete.cv$lambda.min
diabete.cv$lambda.1se

coef(diabete.cv, diabete.cv$lambda.min)
coef(diabete.cv, diabete.cv$lambda.1se)

# 람다 = lambda.min
diabete.gnet1 <- glmnet(x = x, y = y, family = "binomial", alpha = 1, lambda = diabete.cv$lambda.min)
diabete.test.x <- model.matrix(diabetes ~ ., data = diabete.test)[, -1]
diabete.pred1 <- predict(diabete.gnet1, newx = diabete.test.x, type = "response")
diabete.pred1 <- ifelse(diabete.pred1 > 0.5, "pos", "neg")
table(diabete.test$diabetes, diabete.pred1, dnn = c("Actual", "Predicted"))
mean(diabete.pred1 == diabete.test$diabetes) # 정확도: 75.21%

# 람다 = lambda.1se
diabete.gnet2 <- glmnet(x = x, y = y, family = "binomial", alpha = 1, lambda = diabete.cv$lambda.1se)
diabete.test.x <- model.matrix(diabetes ~ ., data = diabete.test)[, -1]
diabete.pred2 <- predict(diabete.gnet2, newx = diabete.test.x, type = "response")
diabete.pred2 <- ifelse(diabete.pred2 > 0.5, "pos", "neg")
table(diabete.test$diabetes, diabete.pred2, dnn = c("Actual", "Predicted"))
mean(diabete.pred2 == diabete.test$diabetes) # 정확도: 75.21%

# 모든 예측변수가 포함된 이항 로지스틱 회귀분석
diabete.logit <- glm(diabetes ~ ., data = diabete.train, family = binomial(link = "logit"))
diabete.logit.pred <- predict(diabete.logit, newdata = diabete.test, type = "response")
diabete.logit.pred <- ifelse(diabete.logit.pred > 0.5, "pos", "neg")
table(diabete.test$diabetes, diabete.logit.pred, dnn = c("Actual", "Predicted"))
mean(diabete.logit.pred == diabete.test$diabetes) # 정확도: 72.64%
