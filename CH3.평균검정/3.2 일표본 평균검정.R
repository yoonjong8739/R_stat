# 3.2 일표본 평균검정
# 하나의 표본 데이터를 이용하여 모집단의 평균이 특정 값과 같은지 검정
library(MASS)
str(cats)

# 귀무가설 : 고양이의 무게는 2.6kg이다.
# 대립가설 : 고양이의 무게는 2.6kg이 아니다.
t.test(cats$Bwt, mu=2.6) # t값이 3.0565 이상 또는 -3.0565 이하가 될 확률이 0.002673에 불과
t.test(cats$Bwt, mu=2.7)

# t.test() 는 기본적으로 양측검정을 수행.

# 귀무가설 : 고양이의 몸무게는 2.6kg보다 작거나 같다.
# 대립가설 : 고양이의 몸무게는 2.6kg보다 크다.
t.test(cats$Bwt, mu=2.6, alternative = "greater")

cats.t <- t.test(cats$Bwt, mu=2.6)
str(cats.t)

cats.t$p.value # p값
cats.t$conf.int # 신뢰구간

# t.test() 는 기본적으로 95% 신뢰구간을 계산
t.test(cats$Bwt, mu=2.6, conf.level = 0.99)  # 99% 신뢰구간

## 모비율 검정 ##
# 귀무가설 : 30경기 중 18승을 했다고 승률 50% 이상이라고 할 수 없다.
# 대립가설 : 30경기 중 18승을 하면 승률 50% 이상이라고 할 수 있다.(채택)
prop.test(x=18, n=30, p=0.5, alternative = "greater")
