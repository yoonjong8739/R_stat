# OLS 회귀분석 종류
# 단순회귀분석: 1개의 독립변수(연속형) & 1개의 종속변수(연속형)
# 다항회귀분석: 1개의 독립변수(연속형) & 1개의 종속변수(연속형), 선형관계는 독립변수의 n차 다항식으로 모델링
# 다중회귀분석: 2개 이상의 연속형 독립변수를 이용하여 1개의 연속형 종속변수를 예측

library(car)
str(Prestige)
head(Prestige)

# 교육기간에 따른  평균 소득 변화
Prestige.lm <- lm(income ~ education, data = Prestige) 
Prestige.lm
class(Prestige.lm)

plot(income ~ education, data = Prestige,
     col = "cornflowerblue", pch = 19, xlab = "Education(year)", ylab = "Income($)",
     main = "Education and Income")
abline(Prestige.lm, col = "salmon", lwd = 2)

library(ggplot2)
ggplot(data = Prestige, aes(x = education, y = income)) +
  geom_point(col = "cornflowerblue") +
  geom_smooth(method = "lm", col = "salmon") + 
  labs(title = "Education and Income")

summary(Prestige.lm)

# 분산분석표
anova(Prestige.lm) # 회귀식의 유의성 검정 활용, 데이터 프레임 형태

rownames(anova(Prestige.lm)); colnames(anova(Prestige.lm))
anova(Prestige.lm)["education", "Pr(>F)"]
anova(Prestige.lm)[1,5]

# 회귀계수
coef(Prestige.lm)
confint(Prestige.lm)
confint(Prestige.lm, level = 0.99)

# 회귀식에 의한 예측값
fitted(Prestige.lm)[1:3]

# 잔차
resid(Prestige.lm)[1:3]

# 새로운 데이터에 대한 예측값 추정
Prestige.new <- data.frame(education = c(5, 10, 15))
predict(Prestige.lm, newdata = Prestige.new)
predict(Prestige.lm, newdata = Prestige.new, interval = "confidence") # 모집단 평균에 대한 95% 신뢰구간

# 일부 서브셋에 대한 회귀분석
# 평균보다 더 많은 교육을 받은 집단과 그렇지 않은 2집단에 대해 각각 회귀분석
mean(Prestige$education)

lm(income ~ education, data = Prestige, subset = (education > mean(education)))
lm(income ~ education, data = Prestige, subset = (education <= mean(education)))

library(dplyr)
edu_mean <- mean(Prestige$education)
Prestige_meanEDU <- Prestige %>% 
  mutate(group = ifelse(education > edu_mean, "Above Mean", "Below or Equal Mean"))
head(Prestige_meanEDU)

ggplot(data = Prestige_meanEDU, aes(education, income, color = group)) +
  geom_point(col = "cornflowerblue") +
  geom_smooth(method = "lm", aes(group = group)) +
  scale_color_manual(values = c("Below or Equal Mean" = "yellowgreen", "Above Mean" = "salmon")) +
  labs(title = "Education and Income", color = "Education Level Group")
