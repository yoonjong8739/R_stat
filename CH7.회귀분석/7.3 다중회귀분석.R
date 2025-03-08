# 종속변수와 하나 이상의 독립변수 간의 선형 관계를 도출하는 통계기법
str(mtcars)
mtcars <- mtcars[c("mpg", "hp", "wt", "disp", "drat")]
summary(mtcars)

library(car)
scatterplotMatrix(mtcars, pch=19, col="royalblue", cex=1.2,regLine = list(method=lm, lty=1, lwd=3, col="salmon"),
            smooth=list(smoother=loessLine, spread=F, lty.smooth=1, lwd.smooth=3, col.smooth="forestgreen"),
            main="Car Performance")

mtcars.lm <- lm(mpg ~ hp+wt+disp+drat, data = mtcars)
summary(mtcars.lm)

#install.packages("stargazer")
library(stargazer)
stargazer(mtcars.lm, type = "text", no.space = T)

# 표준화
mtcars.lm <- lm(scale(mpg) ~ scale(hp) + scale(wt) + scale(disp) + scale(drat), data = mtcars)
summary(mtcars.lm)

# 베타 회귀계수
#install.packages("QuantPsyc")
library(QuantPsyc)
mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data = mtcars)
lm.beta(mtcars.lm) # wt, hp, drat, disp 순으로 종속변수에 미치는 영향이 크다.

# 회귀분석 가정과 진단
# 선형성 : 종속변수와 독립변수 간의 관계는 선형이다.
# 정규성 : 독립변수값에 대응되는 종속변수 값들의 분포는 정규분포이다.
# 등분산성 : 독립변수 값에 대응되는 종속변수 값들의 분포는 모두 동일한 분산을 갖는다.
# 독립성 : 모든 관측값은 서로 독립이다. 하나의 관측값은 다른 관측값에 영향을 미치지 않는다.
mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data = mtcars)
plot(mtcars.lm) 

# 다중공선성: 독립변수 간에 강한 선형관계가 있는 경우
# 다중 회귀분석에서 특정 독립변수의 회귀계수는 다른 독립변수들의 영향을 받지 않아야 한다.
# 다중공선성이 존재하는 경우에는 회귀식이 전반적인 유의성이 확인되어도 개별 회귀계수에 대한 유의성은 나타나지 않거나 회귀계수가 상식에 반하는 부호를 가지기도 한다.
# 다중공선성은 VIF로 측정. 4를 넘으면 점검 필요. 10을 넘으면 다중공선성이 있음.
library(car)
vif(mtcars.lm)

# 회귀모델 수정
# 정규성 가정 위배 : 종속변수 변환
library(car)
summary(powerTransform(mtcars$mpg)) # mpg^0.0296으로 정규화 가능. 그러나 pval이 0.05보다 크기 때문에 변환이 필요 없음.

# 선형성 가정 위배 : 독립변수 변환
boxTidwell(mpg ~ hp + wt, data = mtcars) # hp^-0.56824, wt^-0.41743으로 변환

# 등분산성 가정 위배 : 종속변수 변환
library(car)
spreadLevelPlot(lm(mpg ~ hp + wt, data = mtcars))

# 회귀분석의 주요 목적이 예측이면 다중공선성 문제는 큰 문제가 되지 않는다.
# 그러나 개별 변수에 대한 통계적 해석을 원하면 해결해야 한다.

# 회귀모델 선택 - anova() : 적합도 비교
# 귀무가설:추가된 변수가 회귀모델 예측력 향상에 기여하지 못한다.(채택)
mtcars.lm1 <- lm(mpg ~ hp + wt, data = mtcars)
mtcars.lm2 <- lm(mpg ~ hp + wt + disp + drat, data = mtcars)
anova(mtcars.lm1, mtcars.lm2)

AIC(mtcars.lm1, mtcars.lm2) # AIC 값이 작을수록 우수한 모델
# anova vs AIC : anova(중첩된 모델 간 비교만 가능), AIC(제약 없음)

# 전진선택법, 후진선택법, 단계선택법
mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data = mtcars)
step(mtcars.lm, direction = "backward") # 후진선택법

# 가능한 모든 회귀모델을 탐색하고 각 모델의 적합도를 평가
library(leaps)
mtcars.regsubsets <- regsubsets(mpg ~ hp+wt+disp+drat, data = mtcars, nbest = 4)

library(RColorBrewer)
plot(mtcars.regsubsets, scale = "adjr2", col = brewer.pal(9, "Pastel1"),
     main = "All Subsets Regression") # mpg ~ hp + wt + drat가 가장 설명력이 좋은 모델로 평가

names(summary(mtcars.regsubsets))
summary(mtcars.regsubsets)$adjr2 # 9번째 회귀모델이 가장 설명력이 좋은 모델 
which.max(summary(mtcars.regsubsets)$adjr2)
coef(mtcars.regsubsets, 9)

# 더미변수를 이용한 회귀분석
# 기준범주 : 더미변수 값이 모두 0인 범주
str(InsectSprays)
levels(InsectSprays$spray)
tapply(InsectSprays$count, InsectSprays$spray, mean)

sprays.lm <- lm(count ~ spray, data = InsectSprays)
summary(sprays.lm)
contrasts(InsectSprays$spray) # 더미변수 구조

sprays.aov <- aov(count ~ spray, data = InsectSprays)
summary(sprays.aov)
TukeyHSD(sprays.aov)

respray <- relevel(InsectSprays$spray, ref = 6) # 기준범주 변경
sprays.lm <- lm(count ~ respray, data = InsectSprays)
summary(sprays.lm)
contrasts(relevel(InsectSprays$spray, ref = 6))
