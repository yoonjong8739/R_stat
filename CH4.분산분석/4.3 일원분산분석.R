# 일원분산분석(One way ANOVA)
# 집단을 구분하는 독립변수가 1개인 경우 모집단 간 평균의 동일성 검정
str(InsectSprays)
tapply(InsectSprays$count, InsectSprays$spray, mean)
tapply(InsectSprays$count, InsectSprays$spray, sd)
tapply(InsectSprays$count, InsectSprays$spray, length)

# 집단별 평균 & 신뢰구간
library(gplots)
plotmeans(count ~ spray, data = InsectSprays, barcol = "tomato", barwidth = 3, col = "cornflowerblue", lwd = 2,
          xlab = "Type of Sprays", ylab = "Insect Count", 
          main = "Performance of Insect Sprays\nwith 95% CI of Mean")

boxplot(count ~ spray, data = InsectSprays, col = "tomato",
        xlab = "Type of Sprays", ylab = "Insect Count", main = "Performance of Insect Sprays")

# 일원분산분석
# 귀무가설 : spray 종류별 살아남은 해충의 수 차이가 없다.
# 대립가설 : spray 종류별 살아남은 해충의 수 차이가 있다.(채택)
sprays.aov <- aov(count ~ spray, data = InsectSprays)
sprays.aov
summary(sprays.aov)

# 다중비교, 사후검정(Post-Hoc) : ANOVA 귀무가설 기각 이후 실제 차이가 있는 집단이 구체적으로 어떤 것인지 판단
model.tables(sprays.aov, type = "mean") # 각 집단의 평균 확인 tapply(InsectSprays$count, InsectSprays$spray, mean) 과 동일
model.tables(sprays.aov, type = "effects") # 각 집단평균과 전체평균의 차이 확인

# 터키HSD검정
sprays.compare <- TukeyHSD(sprays.aov)
sprays.compare # p adj에서 하나라도 유의미한 차이가 발생하면 귀무가설을 기각

sprays.compare$spray['D-C',]

plot(TukeyHSD(sprays.aov), col = "blue", las = 1)

#install.packages("multcomp")
library(multcomp)
tuk.hsd <- glht(model = sprays.aov, linfct = mcp(spray="Tukey"))
plot(cld(tuk.hsd, level = 0.05), col = "orange") # 같은 문자를 공유하는 범주는 서로 평균이 다르지 않다.
# A,B,F 살충제는 살충효과가 다르지 않음
# C,D,E 살충제는 살충효과가 다르지 않음

# 분산분석 가정과 진단
# 정규성 : 종속변수는 정규분포를 한다.(Shapiro-Wilk Test, Kolmogorov-Smirnov Test, Q-Q도표)
# 등분산성 : 각 집단의 분포는 모두 동일한 분산을 갖는다.(Levene's Test, Bartlett's Test)
#install.packages("car")
library(car)
qqPlot(InsectSprays$count, pch = 20, col = "deepskyblue", main = "Q-Q Plot", 
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", id = FALSE)

# 정규성 검정
# 귀무가설 : 정규성이다.
# 대립가설 : 정규성이 아니다.(채택)
shapiro.test(InsectSprays$count)

# 이상점 존재여부
# 귀무가설 : 이상점이 없다.
# 대립가설 : 이상점이 있다.(채택)
outlierTest(sprays.aov)

# 등분산 검정
# 귀무가설 : 집단 간 분산이 동일하다(등분산이다).
# 대립가설 : 집단 간 분산이 동일하지 않다(등분산이 아니다).(채택)
leveneTest(count ~ spray, data = InsectSprays)
bartlett.test(count ~ spray, data = InsectSprays)

## 등분산 가정을 충족하지 못할 때 일원분산분석 수행 ##
oneway.test(count ~ spray, data = InsectSprays) # Welch’s ANOVA
# TukeyHSD 사후검정 사용 불가
# Games-Howell Test 사용해야 함.