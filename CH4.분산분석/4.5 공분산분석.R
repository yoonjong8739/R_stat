# 공분산분석
# 분산분석에 공변량을 추가하여 분산분석모델을 확장
#install.packages("faraway")
library(faraway)
str(sexab)

# 아동기 성폭력 경험 유무가 정신건강에 미치는 영향
tapply(sexab$ptsd, sexab$csa, mean)
tapply(sexab$ptsd, sexab$csa, sd)
tapply(sexab$ptsd, sexab$csa, length)

# 아동기의 신체적 학대(cpa)를 공변량으로 투입하여 통제함으로써 아동기의 성폭력 경험과 ptsd 간의 순수한 영향관계 분석
# 귀무가설 : 신체적 학대를 통제한 이후, 아동기의 성폭력 경험은 ptsd와 연관이 없다.
# 대립가설 : 신체적 학대를 통제한 이후, 아동기의 성폭력 경험은 ptsd와 연관이 있다.(채택)
sexab.aov <- aov(ptsd ~ cpa + csa, data = sexab)
summary(sexab.aov)

# 아동기의 신체적 학대(공변량)의 영향을 제거한 후의 조정된 ptsd의 집단평균
#install.packages("effects")
library(effects)
effect(term = "csa", sexab.aov) # tapply(sexab$ptsd, sexab$csa, mean)과 비교

# 종속변수, 공변량, 독립변수 간의 관계를 그래프로 나타내기
library(HH)
ancova(ptsd ~ cpa + csa, data.in = sexab)
