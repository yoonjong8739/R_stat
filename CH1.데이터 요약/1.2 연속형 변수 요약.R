# 중위수
library(MASS)
median(survey$Pulse, na.rm = T)

# 백분위수
quantile(survey$Pulse, probs = 0.05, na.rm = T) # probs 분할하고자하는 비율
quantile(survey$Pulse, probs = 0.5, na.rm = T)
quantile(survey$Pulse, probs = c(0.05, 0.95), na.rm = T)
quantile(survey$Pulse, na.rm = T)

# 특정 값이 주어졌을 때 이에 대응되는 백분위 비율
mean(survey$Pulse <= 80, na.rm = T)

# 왜도
# 평균 > 중위수 = 오른쪽으로 긴 꼬리
mean(survey$Pulse, na.rm = T)
median(survey$Pulse, na.rm = T)

str(iris)

# 요약통계량
summary(iris)
summary(iris$Sepal.Width)
summary(iris$Species)

iris.lst <- as.list(iris) # 리스트 형태로 변환
summary(iris.lst)

lapply(iris.lst, FUN = summary)

# 범위
range(survey$Pulse, na.rm = T)

# 분산
var(survey$Pulse, na.rm = T)

# 표준편차
sd(survey$Pulse, na.rm = T)

# 다양한 기술통계량 함수
summary(mtcars)

#install.packages("pastecs")
library(pastecs)
stat.desc(mtcars[c("mpg", "hp", "wt")])
stat.desc(mtcars[c("mpg", "hp", "wt")], norm = T) # 왜도, 첨도, 정규성검정 결과 산출

library(psych)
describe(mtcars[c("mpg", "hp", "wt")])

# 집단별 기술통계량
tapply(survey$Pulse, survey$Exer, FUN = mean, na.rm=T) # 운동습관별 평균 맥박 수
tapply(survey$Pulse, survey$Sex, mean, na.rm=T) # 성별 평균 맥박 수
tapply(survey$Pulse, list(survey$Exer, survey$Sex), mean, na.rm=T)

aggregate(survey$Pulse, by=list(Exvercise=survey$Exer), FUN=mean, na.rm=T)
aggregate(survey$Pulse, by=list(Excercise=survey$Exer, Sex=survey$Sex), mean, na.rm=T)
aggregate(survey[c("Pulse", "Age")], by=list(Exercise=survey$Exer), mean, na.rm=T)

myStats <- function(x, na.rm=T){
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  mean <- mean(x)
  sd <- sd(x)
  skew <- sum((x-mean)^3 / sd^3) / n
  kurt <- sum((x-mean)^4 / sd^4) / n-3
  return(c(n = n, mean=mean, sd=sd, skewness=skew, kurtosis=kurt))
}

aggregate(survey[c("Pulse", "Age")], list(Exercise=survey$Exer), myStats, na.rm=T)

by(survey[c("Pulse", "Age")], INDICES = list(Exercise=survey$Exer), FUN = summary)
by(survey[c("Pulse", "Age")], list(Exercise=survey$Exer), 
   function(x) sapply(x, myStats, na.rm=T))

library(psych)
describeBy(survey[c("Pulse", "Age")], group = list(Exercise=survey$Exer)) # 사용자 정의 함수는 사용 불가

