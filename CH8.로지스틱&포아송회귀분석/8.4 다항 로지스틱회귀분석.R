# 다항 로지스틱회귀분석 - vglm()
#install.packages("EffectStars")
library(EffectStars)
data(PID)
str(PID)
head(PID)

library(VGAM)
pid.mlogit <- vglm(PID ~ ., data = PID, family = multinomial())
summary(pid.mlogit) # 정치성향은 3개 범주가 있으므로 2개의 회귀모델이 생성(마지막 범주인 Republican을 기준범주로 사용.)
exp(coef(pid.mlogit))

pid.mlogit.pred <- fitted(pid.mlogit)
head(pid.mlogit.pred)

# 다른 독립변수들이 일정할 때, 교육 수준의 차이와 정치 성향의 관계
testdata <- data.frame(Education = c("low", "high"),
                       TVnews = mean(PID$TVnews),
                       Income = mean(PID$Income),
                       Age = mean(PID$Age),
                       Population = mean(PID$Population))
testdata

pid.mlogit.pred <- predict(pid.mlogit, newdata = testdata, type = "response")
cbind(testdata, pid.mlogit.pred)

# 다른 독립변수들이 일정할 때, 소득의 차이와 정치 성향의 관계계
testdata <- data.frame(Education = rep("low", 5),
                       TVnews = mean(PID$TVnews),
                       Income = seq(20,100,20),
                       Age = mean(PID$Age),
                       Population = mean(PID$Population))
testdata

pid.mlogit.pred <- predict(pid.mlogit, newdata = testdata, type = "response")
cbind(testdata, pid.mlogit.pred)

# # 다항 로지스틱회귀분석 - multinom()
library(MASS)
str(fgl)
head(fgl)

fgl.scaled <- cbind(scale(fgl[, 1:9]), fgl[10])

library(caret)
set.seed(123)
train_idx <- createDataPartition(y = fgl.scaled$type, p = 0.7, list = F)
fgl.train <- fgl.scaled[train_idx, ]
fgl.test <- fgl.scaled[-train_idx, ]

table(fgl.train$type)
sum(table(fgl.train$type))

table(fgl.test$type)
sum(table(fgl.test$type))

library(nnet)
fgl.mlogit <- multinom(type ~ ., data = fgl.train)
summary(fgl.mlogit)

z <-summary(fgl.mlogit)$coefficients / summary(fgl.mlogit)$standard.errors
p <- (1-pnorm(abs(z), 0, 1))*2
print(p, digits = 3)

#fgl.mlogit.pred <- predict(fgl.mlogit, newdata = fgl.test, type = "probs")
#cbind(round(fgl.mlogit.pred, 3), fgl.test['type'])

fgl.mlogit.pred <- predict(fgl.mlogit, newdata = fgl.test, type = "class")
head(fgl.mlogit.pred)

table(fgl.test$type, fgl.mlogit.pred, dnn = c("Actual", "Predicted"))
mean(fgl.test$type == fgl.mlogit.pred) # 정확도: 52.459%
