# 반복측정 분산분석
# 하나의 실험 대상에 대해 2번 이상 반복측정한 관측값 간의 차이를 검정
# 반드시 long format 형태를 갖춰야 함

# 반복측정 이원분산분석
# 집단 간 요인 + 집단 내 요인(각 범주별 반복측정) -> 집단변수로서의 역할 수행

head(CO2)
tail(CO2)

co2sub <- subset(CO2, Treatment == "chilled") # 저온처리가 된 나무만을 대상
co2sub$conc <-factor(co2sub$conc)

# 반복측정 일원분산분석 : y ~ W + Error(Subject/W)
# 반복측정 이원분산분석 : y ~ B * W + Error(Subject/W) (B : 집단 간 요인, W : 집단 내 요인)

co2sub.aov <- aov(uptake ~ Type * conc + Error(Plant/conc), data = co2sub)
summary(co2sub.aov)

# 주효과와 상호작용효과 확인
boxplot(uptake ~ Type * conc, data = co2sub, col = c("deepskyblue", "violet"), las = 2, cex.axis = 0.75,
        ylab = "Carboon dioxide uptake rate", main = "Effects of Plant Type and CO2 on Carbon Dioxide Uptake")
legend("topleft", inset = 0.02, legend = c("Quebec", "Mississippi"), fill = c("deepskyblue", "violet"))

library(HH)
interaction2wt(uptake ~ conc*Type, data = co2sub)
