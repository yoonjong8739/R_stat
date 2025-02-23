# 산점도상에 선형회귀선과 LOESS 추세선 추가
library(car)
scatterplot(income ~ education, data = Prestige, pch = 19, col = "orangered", cex = 1.2,
            regLine = list(method = lm, lty = 2, lwd = 3, col = "royalblue"),
            smooth = list(smoother = loessLine, spread = F,
                          lty.smooth = 1, lwd.smooth = 3, col.smooth = "green3"),
            xlab = "Education(years)", ylab = "Income($)", main = "Education and Income")

library(ggplot2)
ggplot(data = Prestige, aes(x = education, y = income)) +
  geom_point(color = "orangered", pch = 19) +
  geom_smooth(method = "lm", lty = 2, lwd = 3, color = "royalblue", se = F) +
  geom_smooth(method = "loess", lty = 1, lwd = 3, color = "green3", se = F) +
  xlab("Education(years)") + ylab("Income($)") + labs(title = "Education and Income") +
  scale_x_continuous(breaks = seq(6, 16, 2)) +
  scale_y_continuous(breaks = seq(0, 25000, 5000), labels = scales::dollar)

# 다항회귀분석은 단순회귀분석을 확장하여 독립변수의 n차 다항식으로 종속변수를 설명
Prestige.ploy <- lm(income ~ education + I(education^2), data = Prestige)
summary(Prestige.ploy)

# 독립변수 간의 상관계수가 크면(다중공선성) 종속변수와 강한 선형관계를 갖는 독립변수로 인해 다른 일부 독립변수의 통계적 유의성이 드러나지 않을 수 있음

plot(Prestige$income ~ Prestige$education, pch = 19, col = "darkorange",
     xlab = "Education(year)", ylab = "Income($)", main = "Education and Income")
library(dplyr)
lines(arrange(data.frame(Prestige$education, fitted(Prestige.ploy)),
              Prestige$education), # 정렬기준
      col = "cornflowerblue", lwd = 2)

ggplot(data = Prestige, aes(x = education, y = income)) +
  geom_point(color = "orangered", pch = 19) +
  geom_smooth(method = "lm", lty = 2, lwd = 3, color = "royalblue", se = F, formula = y ~ poly(x,2)) +
  xlab("Education(years)") + ylab("Income($)") + labs(title = "Education and Income") +
  scale_x_continuous(breaks = seq(6, 16, 2)) +
  scale_y_continuous(breaks = seq(0, 25000, 5000), labels = scales::dollar)

# 간헐천의 분출 대기사간과 분출 지속시간 간의 관계
scatterplot(eruptions ~ waiting, data = faithful, pch = 19, col = "deepskyblue", cex = 1.2,
            regLine = list(method = lm, lty = 2, lwd = 3, col = "blueviolet"),
            smooth = list(smoother = loessLine, spread = F, lty.smooth = 1, lwd.smooth = 3, col.smooth = "coral"),
            xlab = "Waiting(minutes)", ylab = "Eruptions(minutes)",
            main = "Waiting Time Between Eruptions and the Duration of the Eruption")

# 2 군데에서 꺾이는 현상이 나타나기 때문에 3차 다항식 회귀모델을 구축
faithful.poly <- lm(eruptions ~ waiting + I(waiting^2) + I(waiting^3), data = faithful)
summary(faithful.poly)

faithful.lm <- lm(eruptions ~ waiting, data = faithful)
summary(faithful.lm)

# 단순회귀분석 결과보다 개선된 결과가 도출됨.