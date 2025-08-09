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
# 1) AR 모델: 시계열상의 과거 관측값을 이용하여 예측모델을 생성. 예측하고자 하는 특정 변수의 과거 관측값의 선형결합으로 해당 변수의 미래값을 예측.
# 2) MA 모델: 과거 예측오차를 기반으로 예측모델을 구축.

# ARIMA 모델
# 1) 시계열 데이터의 정상성 평가(로그변환, 차분)
# 2) 예측 모델 생성
# 3) 예측 모델 평가와 예측(adf, Acf/Pacf, forecast)

## 1. 시계열 데이터의 정상성 평가
library(tseries)
plot(Nile) # 전 기간에 걸쳐 분산 변동폭이 크지 않아 로그 변환 또는 제곱근 변환이 필요없어 보임.
adf.test(Nile) # 귀무가설: 정상 시계열이 아니다. 대립가설: 정상 시계열이다.

# 정상 시계열이 아니므로 '차분' 필요
library(forecast)
ndiffs(Nile) # 가장 적합한 차분 횟수: 1회
dNile <- diff(Nile, differences = 1)
adf.test(dNile) # 정상 시계열 확인

plot(Nile, col = "darkviolet", lwd = 2, main = "Flow of the River Nile: Original", xlab = "Year", ylab = "Flow")
plot(dNile, col = "dodgerblue", lwd = 2, main = "Flow of the River Nile: Differenced", xlab = "Year", ylab = "Differenced Flow")

## 2. 예측모델 생성
Acf(dNile, lwd = 2, main = "Autocorrelation for the River Nile") # 예측모델 파라미터 결정
Pacf(dNile, lwd = 2, main = "Partial Autocorrelation for the River Nile")

Acf(dNile, plot = F) # 시차 1이후 자기상관이 0이 됨.
Pacf(dNile, plot = F) # 시차 2이후 자기상관이 0이 됨.
# ARMA(2,0), ARMA(0, 1), ARMA(2,1) 등의 모델이 가능
# 간명도의 원칙: 성능이 비슷한 경우, 파라미터 수가 적은 모델을 선택.

Nile.arima <- arima(Nile, order = c(0,1,1)) # p=0, d(차분 횟수)=1, q=1
Nile.arima
accuracy(Nile.arima)

## 3. 예측모델 평가와 예측
# 잔차의 정규성 확인
hist(Nile.arima$residuals, col = "mistyrose", prob = T,
     main = "Histogram of Residuals", xlab = "Residuals")
xfit <- seq(min(Nile.arima$residuals), max(Nile.arima$residuals), length.out = 40)
yfit <- dnorm(xfit, mean = mean(Nile.arima$residuals), sd = sd(Nile.arima$residuals))
lines(xfit, yfit, col = "tomato", lwd = 2)

qqnorm(Nile.arima$residuals, pch = 21, col = "black", bg = "gold", main = "Q-Q Plot of Residuals")
qqline(Nile.arima$residuals, col = "royalblue", lwd = 2)

# 잔차의 독립성(자기상관 = 0) 확인
# 귀무가설: 잔차는 자기상관이 0이다.(잔차는 독립적이다.)
# 대립가설: 잔차는 자기상관이 0이 아니다. (잔차는 독립적이지 않다.)
Box.test(Nile.arima$residuals, type = "Ljung-Box")
Box.test(Nile.arima$residuals)

Nile.arima.pred <- forecast(Nile.arima, h = 5)  # 향후 5년치 나일강 유량
Nile.arima.pred

plot(Nile.arima.pred, col = "darkgreen", lwd = 2, flty = 1, flwd = 3,
     fcol = "royalblue", shadecols = c("mistyrose", "salmon"), # 80%, 95% 신뢰구간
     main = "Forecast for Flow of the River Nile",
     xlab = "Year", ylab = "Flow")

# 계절성분과 모델 자동 선택
library(forecast)
gas # 1956년 ~ 1995년까지의 호주의 월별 가스 생산량 시계열 데이터

gas.arima <- auto.arima(gas) # 계절 성분을 포함한 ARIMA 모델
gas.arima
arima(gas, order = c(2,1,1), seasonal = list(order = c(0,1,1), period = 12)) # auto.arima(gas)와 동일

forecast(gas.arima, h = 5*12) # 향후 5년간 월별 가스 생산량
plot(forecast(gas.arima, h = 5*12), col = "darkorange", lwd = 2,
     flty = 1, flwd = 3, fcol = "orangered", shadecols = c("lavender", "skyblue"),
     main = "Australian Monthly Gas Production", xlab = "Year", ylab = "Monthly Production")

library(ggfortify)
library(scales)
autoplot(forecast(gas.arima, h = 5*12), ts.colour = "cornflowerblue", ts.size = 1,
         predict.colour = "salmon", predict.linetype = "solid",
         predict.size = 1, conf.int.fill = "tomato") +
  scale_y_continuous(labels = comma) +
  labs(x = "", y = "Monthly Production", title = "Australian Monthly Gas Production") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.line = element_line(),
        axis.ticks = element_line(),
        axis.text.x = element_text(size = 10))
