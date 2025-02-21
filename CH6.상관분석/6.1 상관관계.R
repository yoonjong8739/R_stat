# 상관관계
# 2개의 연속형 변수의 관계에 대한 분석
library(MASS)
str(cats)

# 산점도
plot(cats$Hwt ~ cats$Bwt, col = "forestgreen", pch = 19, xlab = "Body Weight", ylab = "Heart Weight(g)",
     main = "Body Weight and Heart Weight of Cats")

library(ggplot2)
ggplot(data = cats, aes(x = Bwt, y = Hwt, colour = Sex)) +
  geom_point()

ggplot(data = cats, aes(x = Bwt, y = Hwt, colour = Sex)) +
  geom_point() + geom_smooth(method = "lm", lty = 1, linewidth = 2)

# 상관계수
cor(cats$Bwt, cats$Hwt) # 피어슨 상관계수
cor(cats$Bwt, cats$Hwt, method = "spearman") # 스피어만 상관계수
cor(cats$Bwt, cats$Hwt, method = "kendall")  # 켄달 상관계수

# 상관계수 계산 시 결측치 처리 방법
cor(cats$Bwt, cats$Hwt, use = "everything") # 결측값이 있는 경우 NA 출력
cor(cats$Bwt, cats$Hwt, use = "complete.obs") # 하나라도 결측값을 포함하고 있는 케이스는 상관계수 계선에서 제외
cor(cats$Bwt, cats$Hwt, use = "pairwise.complete.obs") # 분석에 사용되는 변수에 대해서만 결측값이 존재할 때 해당 케이스를 분석에서 제외

# 피어슨 상관계수는 정규성 가정을 필요로 함.
# 정규성 가정을 충족하지 못할 경우, 스피어만 상관계수(서열척도)를 사용.

# 상관계수 유의성 검정
# 귀무가설: Bwt와 Hwt 변수 간에 상관관계가 없다.
# 대립가설: wt와 Hwt 변수 간에  상관관계가 있다.(채택)
with(cats, cor.test(Bwt, Hwt))

# 귀무가설: Bwt와 Hwt 변수 간에 상관관계가 없거나 음의 상관이 있다.
# 대립가설: wt와 Hwt 변수 간에  양의 상관관계가 있다.(채택)
with(cats, cor.test(Bwt, Hwt, alternative="greater", conf.level=0.99))

# 포뮬러 형식
with(cats, cor.test(~ Bwt + Hwt))
cor.test(~ Bwt + Hwt, data = cats, subset = (Sex == "F")) # 암컷 고양이에 대해서만 상관계수 유의성 검정

# 상관행렬
str(iris)
cor(iris[-5])

iris.cor <- cor(iris[-5])
class(iris.cor)
str(iris.cor)
iris.cor["Petal.Width", "Petal.Length"]

# 2개 이상의 변수 간의 상관계수 유의성 검정
library(psych)
corr.test(iris[-5])
print(corr.test(iris[-5]), short = F) # 신뢰구간 95%

# 상관행렬 산점도 행렬
options(digits = 2)
cor(state.x77)

library(psych)
pairs.panels(state.x77, bg = "red", pch = 21, hist.col = "gold", 
             main = "Correlation Plot of US States Data")

#install.packages("corrgram")
library(corrgram)
corrgram(state.x77, order = T, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt,
         main = "Corrgram of US States Data")

cols <- colorRampPalette(c("darkgoldenrod4", "burlywood1", "darkkhaki", "darkgreen"))
corrgram(state.x77, order = F, col.regions = cols,
         lower.panel = panel.pie, upper.panel = panel.conf, text.panel = panel.txt,
         main = "Corrgram of US States Data")
