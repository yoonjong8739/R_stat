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
lm.beta(mtcars.lm)
