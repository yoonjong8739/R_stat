# 매개효과 분석
# 매개변수: 두 변수 간 영향관계가 발생하는 이유, 즉 두 변수 간의 관계에 내재된 간접적인 영향관계 프로세스를 설명
# X가 M을 통해 Y에 영향을 미치는 과정
# 어떻게 X가 Y에 영향을 미치는가?
# 바론 & 케니 절차에 따라 수행: 매개효과의 존재 여부 확인 후 Sobel 또는 bootstrapping을 이용하여 간접효과에 대한 통계적 유의성을 검정.
# 1단계: X -> Y (회귀계수가 통계적으로 유의해야 함. 그렇지 않으면, 다음 단계 진행 불가)
# 2단계: X -> M (회귀계수가 통계적으로 유의해야 함. 그렇지 않으면, M은 또 하나의 독립변수에 불과)
# 3단계: X + M -> Y (X를 통제하고 M과 Y의 회귀계수가 통계적으로 유의해야 매개효과가 존재.)
####
#예시
#상황: "부모의 스트레스(X)가 자녀의 학업 성취도(Y)에 영향을 미친다."
#매개변수: "부모의 양육 방식(M)"
#분석: 부모의 스트레스가 양육 방식을 변화시키고(M), 그 양육 방식이 자녀의 학업 성취도(Y)에 영향을 미치는지 확인.
#결과:
#직접효과: 부모 스트레스가 자녀 성적에 직접 영향을 줄 수 있음.
#간접효과: 스트레스 때문에 양육 방식이 바뀌고, 그 결과 자녀 성적이 달라질 수 있음
####

# 1단계. 독립변수(disp)와 종속변수(mpg)의 관계 검정
data("mtcars")
model.total <- lm(mpg ~ disp, data = mtcars)
summary(model.total)

# 2단계. 독립변수(disp)와 매개변수(wt)의 관계 검정
model.M <- lm(wt ~ disp, data = mtcars)
summary(model.M)

# 3단계. 독립변수(disp)를 통제한 상태에서의 매개변수(wt)와 종속변수(mpg)의 관계 검정
model.Y <- lm(mpg ~ disp + wt, data = mtcars)
summary(model.Y) # 자동차 무게(wt)는 배기량(disp)과 연비(mpg)간의 관계를 완전매개한다. 

# 단 이 방법으로는 매개변수(wt)에 의한 간접효과의 통계적 유의성을 검정하지 않으므로, 이러한 영향관계의 변화가 유의한지는 알 수 없음.
# Sobel과 bootstrapping을 이용하여 간접효과에 대한 통계적 유의성을 검정 가능.

# Sobel 검정
#install.packages("multilevel")
library(multilevel)
model.sob <- sobel(pred = mtcars$disp, # 독립변수
                   med = mtcars$wt,    # 매개변수
                   out = mtcars$mpg)   # 종속변수
model.sob
pnorm(abs(model.sob$z.value), lower.tail = F)*2  # 간접효과에 대한 p-값

#install.packages("bda")
library(bda)
mediation.test(mv=mtcars$wt,   # 매개변수
               iv=mtcars$disp, # 독립변수
               dv=mtcars$mpg)  # 종속변수
# Sobel 검정: 간접효과가 정규분포를 따라야 한다. 표본크기가 커야 한다.

# 부트스트래핑: 다수의 무작위 표본을 생성하여 간접효과를 추정.
#install.packages("mediation")
library(mediation)
set.seed(123)
model.M <- lm(wt ~ disp, data = mtcars)        # 매개변수 모델
model.Y <- lm(mpg ~ disp + wt, data = mtcars)  # 종속변수 모델

model.mediation <- mediate(model.m = model.M,  # 매개변수 모델
                           model.y = model.Y,  # 종속변수 모델
                           treat = "disp",     # 독립변수
                           mediator = "wt",    # 매개변수
                           boot = T,           # 부트스트래핑
                           sims = 500)         # 추출할 표본 개수

# Total Effect: 독립변수가 종속변수에 미치는 영향(매개변수가 포함되지 않는 회귀모델), lm(mpg ~ disp, data = mtcars)
# ADE: 직접효과, 독립변수가 종속변수에 미치는 영향(매개변수 영향을 고려한 회귀모델), lm(mpg ~ disp + wt, data = mtcars)
# ACME: 간접효과, Total Effect-ADE
summary(model.mediation)
plot(model.mediation, cex=1.2, col="royalblue", lwd=2, main="Mediation Effect Analysis")

# 조절효과분석
# 조절변수: 두 변수 간 관계를 강화시킬 수도, 약화시킬 수도 있음.
# 조절효과 = 상호작용효과
# M이 X와 Y 관계를 강화 또는 약화시키는 조건
# 언제 어떤 조건에서 X가 Y에 영향을 미치는가?
mtcars.lm <- lm(mpg ~ hp + wt + hp:wt, data = mtcars)
summary(mtcars.lm) # hp와 wt 사이에 상호작용이 존재함. 즉, hp와 mpg 간의 관계 패턴은 wt에 따라 변동됨.

# 상호작용효과 확인
library(effects)
m <- round(mean(mtcars$wt), 1); m
s <- round(sd(mtcars$wt), 1); s
plot(effect(term = "hp:wt", mod = mtcars.lm, xlevels=list(wt = c(m-s, m, m+s))),
     lines = list(multiline = T, lwd = 2, lty = c(3, 2, 1),
                  col = c("royalblue", "violet", "maroon")),
     main = "Interaction Plot for Horsepower and Weight")

#install.packages("rockchalk")
library(rockchalk)
plotSlopes(model = mtcars.lm, # 조절효과 회귀모델
           plotx = "hp",      # 독립변수
           modx = "wt",       # 조절변수
           modxVals = "std.dev", # 평균에서 1 표준편차만큼 떨어진 값일 때의 회귀선
           pch = 21, col = rainbow(3), cex = 1, bg = "dimgray",
           main = "Interaction Plot for Horsepower and Weight")

# 조절내개효과분석
# 매개변수에 의해 매개된 두 변수(독립,종속) 간 직접적 또는 간접적 영향관계에 제4의 변수(조절변수)가 영향을 미치는지 검정
# 훈련기간(독립), 취업기회(종속), 기술습득(매개), 자기효능감(조절)

# 조절매개효과분석은 매개효과와 조절효과를 동시에 검정한다.
model.M <- lm(wt ~ disp*am, data = mtcars) # 매개변수모델(X -> M) + 조절변수
model.Y <- lm(mpg ~ disp*am + wt*am, data = mtcars) # 종속변수모델(X+M -> Y) + 조절변수

library(mediation)
set.seed(12)
model.med1 <- mediate(model.m = model.M, model.y = model.Y, covariates = list(am = 0),
                       treat = "disp", mediator = "wt", boot = T, sims = 500)
summary(model.med1)

set.seed(12)
model.med2 <- mediate(model.m = model.M, model.y = model.Y, covariates = list(am = 1),
                       treat = "disp", mediator = "wt", boot = T, sims = 500)
summary(model.med2)

# 간접효과의 차이가 통계적으로 유의한지 검정
# 1. 상호적용항이 포함된 매개변수모델과 종속변수모델로 매개효과분석 수행
set.seed(12)
model.med <- mediate(model.m = model.M, model.y = model.Y, 
                     treat = "disp", mediator = "wt", boot = T, sims = 500)

# 2. test.modmed()로 매개효과모델에서의 조절효과를 검정
set.seed(12)
test.modmed(object = model.med, # 매개효과분석 결과
            covariates.1 = list(am=0), # 조절변수 1수준
            covariates.2 = list(am=1), # 조절변수 2수준
            sims=500)

# 배기량(disp)이 자동차 무게(wt)를 매개로 해서 연비(mpg)에 미치는 영향은 변속기 유형(am)에 따라 차이가 있다.
# 배기량(disp)이 연비(mpg)에 미치는 직접적인 영향은 변속기 유형(am)에 따라 차이가 없다.