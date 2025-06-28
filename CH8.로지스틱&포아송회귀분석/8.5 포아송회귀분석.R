# 포아송회귀분석: 결과변수가 특정 기간 동안의 사건발생횟수인 경우에 적용 가능.
# 예: 1시간 동안 걸려오는 상담전화횟수, 하루동안 발생하는 범죄 횟수, 1달 동안 발생하는 교통사고횟수 등
#install.packages("robust")
library(robust)
data("breslow.dat") # 투약 전 8주간 발작횟수 vs 투약 후 8주간 발작횟수
str(breslow.dat)
head(breslow.dat)

seizure <- breslow.dat[c("Base", "Age", "Trt", "sumY")]
summary(seizure)
hist(seizure$sumY, breaks = 20, col = "cornflowerblue",
     xlab = "Seizure Count", main = "Distribution of Seizures")

seizure.poisson <- glm(sumY ~ ., data = seizure, family = poisson())
summary(seizure.poisson)
coef(seizure.poisson)
exp(coef(seizure.poisson))

# 평균과 분산이 동일
# 과산포 문제: 분산-평균 비율이 1보다 클 때
# 데이터에 과산포 존재 시, 표준오차가 매우 작아지고 유의확률을 지나치게 작게 만들어서 회귀계수가 0이라는 귀무가설을 쉽게 기각
# 포아송분포는 각 사건이 독립사건이고 사건발생률이 일정하다고 가정 -> 현실적이지 못함.
# 로지스틱 회귀분석처럼 잔차이탈도 / 잔차자유도의 비율이 1보다 크면 과산포를 의심.
deviance(seizure.poisson) / df.residual(seizure.poisson)

# 과산포 가능성을 통계적으로 검정.
#install.packages("qcc")
library(qcc)
qcc.overdispersion.test(seizure$sumY, type = "poisson") # 과산포 문제가 있다.

# 과산포 문제가 있을 경우 quasipoisson() 으로 해결
# 표준오차가 커짐 -> 표준오차가 커지면 유의확률 p값을 증가시킴.
seizure.qpoisson <- glm(sumY ~ ., data = seizure, family = quasipoisson())
summary(seizure.qpoisson) # Base(과거 발작횟수) 변수만 유의미 함.

# 각 관측값의 시간 간격이 다른 경우의 포아송회귀분석
# 단위 시간 당 사건 발생횟수를 결과변수로 사용.
#glm(sumY ~ ., data = seizure, family = poisson(), offset = log(time))

library(MASS)
str(ships)
shipsinc <- subset(ships, service > 0)
shipsinc$year <- factor(shipsinc$year)
shipsinc$period <- factor(shipsinc$period)
levels(shipsinc$year)
levels(shipsinc$period)

shipsinc.poisson <- glm(incidents ~ ., data = shipsinc, family = poisson(), offset = log(service))
summary(shipsinc.poisson)

deviance(shipsinc.poisson) / df.residual(shipsinc.poisson) # 과산포 검정
qcc.overdispersion.test(shipsinc$incidents, type = "poisson") # 과산포 문제가 있음

shipsinc.qpoisson <- update(shipsinc.poisson, family = quasipoisson())
summary(shipsinc.qpoisson)

exp(coef(shipsinc.qpoisson))
