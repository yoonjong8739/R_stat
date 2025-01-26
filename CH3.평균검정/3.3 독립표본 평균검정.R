# 3.3 독립표본 평균검정
# 2개의 독립표본 데이터를 이용하여 각각 대응되는 2개의 모집단 평균이 서로 동일한지 검정
library(MASS)

# 귀무가설 : 성별에 따른 고양이 몸무게 차이는 없다.
# 대립가설 : 성별에 따른 고양이 몸무게 차이가 있다.(채택)
t.test(data=cats, Bwt ~ Sex)

bars <- tapply(cats$Bwt, cats$Sex, mean)
lower <- tapply(cats$Bwt, cats$Sex, function(x) t.test(x)$conf.int[1])
upper <- tapply(cats$Bwt, cats$Sex, function(x) t.test(x)$conf.int[2])

#install.packages("gplots")
library(gplots)
# 신뢰구간이 겹치지 않음 -> 성별에 따른 몸무게 차이가 존재한다.
barplot2(bars, space = 0.4, ylim = c(0, 3),
         plot.ci = T, ci.l = lower, ci.u = upper, ci.color = "maroon", ci.lwd = 4,
         names.arg = c("Female", "Male"), col = c("coral", "darkkhaki"),
         xlab = "Cats", ylab = "Body Weight (kg)",
         main = "Body Weight by Sex\nwith Confidence Interval")

Bwt.f <- cats$Bwt[cats$Sex == "F"]
Bwt.m <- cats$Bwt[cats$Sex == "M"]
t.test(Bwt.f, Bwt.m)

## 폐질환 대비 흡연자의 비율이 4개의 병원에서 모두 동일한가? ##
# 귀무가설 : 4개 병원의 폐질환 대비 흡연자 비율은 차이가 없다.
# 대립가설 : 4개 병원의 폐질환 대비 흡연자 비율의 차이가 있다.(채택)
smoker <- c(83, 90, 129, 70)
patients <- c(86, 93, 136, 82)
prop.test(smoker, patients)
