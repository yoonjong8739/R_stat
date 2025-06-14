#install.packages("C50")
#install.packages("modeldata")
library(C50)
library(modeldata)
data(mlc_churn)
str(mlc_churn)

# 데이터셋 분할하기(7:3)
library(caret)
churnTrain_idx <- createDataPartition(y = mlc_churn$churn, p = 0.7, list = F)
churnTrain <- mlc_churn[churnTrain_idx, ]
churnTest <- mlc_churn[-churnTrain_idx, ]

str(churnTrain)
str(churnTest)

churn.train <- churnTrain[-c(1,3)]
churn.train$churn <- factor(ifelse(churn.train$churn == "no", 1, 2),
                           levels = c(1,2), labels = c("no", "yes"))
str(churn.train)

levels(churn.train$churn) # 1: no, 2: yes
table(churn.train$churn) # 약 14% 이탈
prop.table(table(churn.train$churn))

# 로지스틱 회귀분석: 고객 이탈(2) 확률이 고객 미이탈(1) 확률의 몇 배인가?
churn.logit <- glm(churn ~ ., data = churn.train, family = binomial(link = "logit"))
summary(churn.logit)

exp(coef(churn.logit)) # 오즈비. 2만큼 늘어나면 제곱으로 계산

# 테스트 데이터(30%)에 적용
churn.test <- churnTest[-c(1,3)]
churn.test$churn <- factor(ifelse(churn.test$churn == "no", 1, 2),
                           levels = c(1,2), labels = c("no", "yes"))
churn.logit.pred <- predict(churn.logit, newdata = churn.test, type = "response")
head(churn.logit.pred)

churn.logit.pred <- factor(churn.logit.pred > 0.5, levels = c(FALSE, TRUE), labels = c("no", "yes"))
head(churn.logit.pred)
table(churn.logit.pred)

# 혼동행렬
table(churn.test$churn, churn.logit.pred, dnn = c("Actual", "Predicted"))
mean(churn.test$churn == churn.logit.pred) # 정확도: 85.6571%

## 단계별 로지스틱회귀분석 ##
churn.logit2 <- step(churn.logit)
summary(churn.logit2)

churn.logit2.pred <- predict(churn.logit2, newdata = churn.test, type = "response")
head(churn.logit2.pred)

churn.logit2.pred <- factor(churn.logit2.pred > 0.5, levels = c(FALSE, TRUE), labels = c("no", "yes"))
head(churn.logit2.pred)
table(churn.logit2.pred)

table(churn.test$churn, churn.logit2.pred, dnn = c("Actual", "Predicted"))
mean(churn.test$churn == churn.logit2.pred) # 85.39026%

# 과산포 확인 (과산포 있으면, family = quasibinomial()을 지정)
deviance(churn.logit2) / df.residual(churn.logit2) # 잔차이탈도/잔차자유도 1보다 작으면 과산포 위험은 없음.

# family = binomial() 한 glm과 family = quasibinomial()을 지정한 glm 간의 카이제곱 검정을 하면 통계적 유의성을 확인할 수 있음.