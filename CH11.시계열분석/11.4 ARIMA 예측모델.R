# ARIMA 예측값 = 관측값과 예측오차의 선형결합으로 표현

## 정상성과 자기상관
# 시계열 데이터의 정상성을 가정 (데이터의 특성이 시간의 흐름에 따라 변하지 않음).
# 추세, 계절요인은 시간이 경과하면서 관측값에 영향을 미치기 때문에 두 성분을 갖는 시계열은 비정상적. 
# 불규칙 성분만으로 구성된 시계열은 정상적.
# 일반적으로 정상 시계열은 장기적으로 예측 가능한 패턴을 갖지 않음.
# 평균과 분산이 일정. 관측값 간의 공분산도 일정.
# 추세나 계절 요인이 포함되어 있어서 데이터가 정상성을 갖지 않으면 복잡한 패턴을 모델링하여 분석하는 것이 어렵기 때문에 일반적으로 정상성을 갖도록 전처리하게 됨.

library(fpp2)
plot(AirPassengers, col = "red", lwd = 2, main = "(a) Air Passengers", xlab = "Year", ylab = "Persons (1,000)")
plot(goog200, col = "blue", lwd = 2, main = "(b) Google Stock Prices", xlab = "Day", ylab = "Dollars")
plot(Nile, col = "green3", lwd = 2, main = "(c) Flow of the River Nile", xlab = "Year", ylab = "Flow")
plot(nottem, col = "mediumorchid4", lwd = 2, main = "(d) Temperatures at Nottingham", xlab = "Year", ylab = "Fahrenheit")

# 자기상관은 시차(lag)를 적용한 시계열 데이터를 이용하여 계산
Nile # 1871년 ~ 1970년 나일강 연간 유량

lag(Nile, 1) # 시차1 이동(1년 뒤로 이동)
lag(Nile, 2) # 시차2 이동(2년 뒤로 이동)

# 시차가 커질수록 예측하기 어려워짐
# ACF(자기상관함수): 시차에 따른 관측값 간의 연관 정도
# 시차가 크면 ACF는 0에 가까워짐.
# 시계열의 정상성 평가에 유용.

# 편자기상관(PACF)
# 시차가 다른 두 시계열 데이터 간의 순수한 상호 연관성
# 두 시점 사이에 포함된 모든 시계열 데이터의 영향은 제거.
# ACF, PACF 도표는 ARIMA 모델의 파라미터를 결정하고 모델의 적합도를 평가하는 목적으로 사용.

# ARIMA 모델은 정상 시계열을 가정
# 그렇지 않은 경우 로그 변환, 제곱근 변환 등의 변환 과정 필요.
# 비정상 시계열 데이터는 '차분' 과정을 통해 정상 시계열로 변환.

library(fpp2)
library(forecast)
head(goog200)
ndiffs(goog200) # 가장 적합한 차분 횟수
dgoog200 <- diff(goog200, differences = 1) # 정상 시계열로 변환
head(dgoog200)

plot(goog200, col = "cornflowerblue", lwd = 2, main = "(a) Google Stock Prices", xlab = "Day", ylab = "Dollars")
plot(dgoog200, col = "salmon", lwd = 2, main = "(b) Google Stock Prices\nTransformed by Differencing", xlab = "Day", ylab = "Dollars")
Acf(goog200, lwd = 2, main = "Original Data")
Acf(dgoog200, lwd = 2, main = "Differenced Data")

library(tseries)
adf.test(goog200) # ADF 검정: 정상성 평가(귀무가설: 정상 시계열이 아니다. 대립가설: 정상 시계열이다.)
adf.test(dgoog200)

# 비정상 시계열을 정상 시계열로 변환하는 경우
# 1) 변동폭이 일정하지 않을 경우: 로그 변환(분산 일정하게 유지)
# 2) 추세 or 계절 요인이 있을 경우: 차분(평균 일정하게 유지)
# 3) 변동폭이 일정하지 않고 추세와 계절적 요인이 존재하는 경우: 로그 변환 + 차분
old.par <- par(mfrow = c(2, 2))
plot(AirPassengers, col = "green3", lwd = 2, main = "Non-stationary Time Series", 
     xlab = "Time", ylab = "Air Passengers")
plot(log(AirPassengers), col = "purple2", lwd = 2, main = "Constant Variance", 
     xlab = "Time", ylab = "log(Air Passengers)")
plot(diff(AirPassengers, differences = 1), col = "pink2", lwd = 2, main = "Constant Mean", 
     xlab = "Time", ylab = "diff(Air Passengers)")
plot(diff(log(AirPassengers), differences = 1), col = "salmon", lwd = 2, main = "Stationary Time Series", 
     xlab = "Time", ylab = "diff(log(Air Passengers))")
par(old.par)

# ARMA 모델과 ARIMA 모델





