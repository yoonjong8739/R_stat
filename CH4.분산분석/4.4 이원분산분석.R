# 이원분산분석(Two way ANOVA)
# 집단을 구분하는 독립변수가 2개인 경우에 모집단 간 동일성을 검정
# 2개의 주효과와 1개의 상호작용 효과를 검정
# 주효과 : 각 독립변수가 개별적으로 종속변수에 유의한 영향을 미치는지 검정
# 상호작용 효과 : 두 독립변수의 조합이 종속변수와 유의한 영향을 미치는지 검정

# 10마리의 기니피그를 대상으로 비타민C가 이빨의 성장에 영향을 미치는가?
str(ToothGrowth)
ToothGrowth$dose <- factor(ToothGrowth$dose, levels = c(0.5, 1.0, 2.0), labels = c("low", "med", "high"))
str(ToothGrowth)

ToothGrowth[seq(1, 60, 5),]

# 각 집단의 크기와 집단별 평균 및 표준편차를 계산
# with(data = ToothGrowth, tapply(len, list(supp, dose), length))
tapply(ToothGrowth$len, list(ToothGrowth$supp, ToothGrowth$dose), length) 
tapply(ToothGrowth$len, list(ToothGrowth$supp, ToothGrowth$dose), mean)
tapply(ToothGrowth$len, list(ToothGrowth$supp, ToothGrowth$dose), sd)

# 이원분산분석
# 귀무가설1 : 기니피그의 이빨 성장은 보충제의 종류와 상관없다.
# 귀무가설2 : 기니피그의 이빨 성장은 투여량과 상관없다.
# 귀무가설3 : 기니피그의 이빨 성장은 보충제와 투여량의 조합과 상관없다.

# 대립가설1 : 기니피그의 이빨 성장은 보충제의 종류와 상관있다.
# 대립가설2 : 기니피그의 이빨 성장은 투여량과 상관있다.
# 대립가설3 : 기니피그의 이빨 성장은 보충제와 투여량의 조합과 상관있다.
ToothGrowth.aov <- aov(len ~ supp*dose, data = ToothGrowth)
ToothGrowth.aov
summary(ToothGrowth.aov)

model.tables(ToothGrowth.aov, type = "means")
model.tables(ToothGrowth.aov, type = "effects")
# OJ가 VC보다 기니피그 이빨 성장에 영향을 미친다.
# 투여량이 높을수록 기니피그 이빨 성장에 영향을 미친다.
# OJ가 이빨 성장에 큰 영향을 미치지만 투여량이 많을수록 달라진다.

boxplot(len ~ supp*dose, data = ToothGrowth,
        col = c("deeppink", "yellowgreen"), las = 1,
        xlab = "Vitamin C Type", ylab = "Tooth Growth",
        main = "Effects of Vitamin C on Tooth Growth of Guinea Pigs")
# 투여량이 작거나 중간인 경우에는 OJ의 영향이 더 크지만, 투여량이 많은 경우에는 두 보충제의 영향이 비슷해짐.

# 주효과와 상호작용효과 시각화
interaction.plot(x.factor = ToothGrowth$dose, # X축에 위치할 집단변수
                 trace.factor = ToothGrowth$supp, # 선으로 그려질 집단변수
                 response = ToothGrowth$len, las = 1, # 반응변수(종속변수)
                 type = "b", pch = c(1,19), col = c("blue", "red"),
                 trace.label = "Supplement", # 범례 제목
                 xlab = "Dose Level", ylab = "Tooth Length", main = "Intersection Plot for Tooth Growth of Guinea Pigs")
# 보충제 종류와 투여량의 상호작용효과가 있다.

# 평균도표
library(gplots)
plotmeans(len ~ interaction(supp, dose, sep = " "), data = ToothGrowth, connect = list(c(1,3,5), c(2,4,6)),
          col = c("red", "green3"), xlab = "Supplement and Dose Combination", ylab = "Tooth Length",
          main = "Means Plot for Tooth Growth of Guinea Pigs\nwith 95% CI of Mean")

# 조건부도표
coplot(len ~ dose | supp, data = ToothGrowth,
       col = "steelblue", pch = 19,
       panel = panel.smooth, lwd = 2, col.smooth = "darkorange",
       xlab = "Dose Level", ylab = "Tooth Length")

# 주효과, 상호작용효과를 동시에 볼 수 있는 그래프
#install.packages("HH")
library(HH)
interaction2wt(len ~ supp * dose, data = ToothGrowth)

# 사후검정
TukeyHSD(ToothGrowth.aov)
