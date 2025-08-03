# 비계열 데이터 분해
# 위 아래로 크게 움직이는 불규칙 변동요인이 있음
nhtemp # 1912년 ~ 1971년까지 연평균 기온(화씨) 시계열 데이터
plot(nhtemp)

library(forecast)
windows(width = 7, height = 5.5)
old.par <- par(mfrow = c(2, 2))
ylim <- c(min(nhtemp), max(nhtemp))
plot(nhtemp, ylim = ylim, col = "dimgray", lwd = 2,
     main = "Base Time Series", ylab = "Temperature")

plot(ma(nhtemp, 3), ylim = ylim, col = "red", lwd = 2,
     main = "단순이동평균(k=3)", ylab = "Temperature")

plot(ma(nhtemp, 7), ylim = ylim, col = "green3", lwd = 2,
     main = "단순이동평균(k=7)", ylab = "Temperature")

plot(ma(nhtemp, 11), ylim = ylim, col = "blue", lwd = 2,
     main = "단순이동평균(k=11)", ylab = "Temperature")
par(old.par)

# 계절 데이터 분해
# 1보다 큰 주기를 갖는 시계열 데이터
# 추세 성분, 계절 성분, 불규칙 성분으로 분해

# 추세 성분: 시간의 흐름에 따른 수준의 변화.
# 계절 성분: 단위 기간 내에서의 순환 주기의 영향을 설명.
# 불규칙 성분: 추세, 계절 성분에 의해 설명되지 않는 영향을 포착.

# 성분 분해 방법
# 1) 가법적: 추세 + 계절 + 불규칙 성분의 합
# 2) 승법적: 추세 * 계절 * 불규칙 성분의 곱

# 가법모델
# decompose(): 이동평균을 이용한 성분분해
# stl(): LOESS 평활법 사용(가법모델만 지원 - 로그 변환으로 승법모델 적용)
co2 <- window(CO2, start = c(1985, 1), end = c(1996, 12))
co2

# 최소 7이상의 홀수 지정. 계절효과가 전 시계열 기간에 걸쳐 모두 동일하도록 설정.
co2.decomp <- stl(co2, s.window = "periodic")
co2.decomp

plot(co2.decomp, col = "darkcyan", col.range = "skyblue", lwd = 2,
     main = "Decomposition of CO2 Concentration Time Series")

co2.decomp$time.series  # 추세, 계절, 불규칙 성분으로 분해
co2.adj <- co2 - co2.decomp$time.series[, "seasonal"]  # 계절 성분의 기여분을 제거(가법모델로 설명 가능한 시계열 데이터의 경우에만 한정)
co2.adj

# 계절 효과가 제거된 시계열 그래프
plot(co2.adj, col = "tomato", lwd = 2,
     main = "CO2 Concentration Time Series without Seasonal Effect", 
     xlab = "Year", ylab = "CO2 Concentration (Parts per Million)")

# 계절효과 자세히 살펴보기
library(forecast)
monthplot(co2, col = "slateblue", lwd = 2, main = "Month Plot",
          xlab = "Month", ylab = "CO2 Concentration (Parts per Million)") # 월별 서브셋 시계열 그래프
seasonplot(co2, col = "sienna", lwd = 2, year.labels = T, main = "Season Plot",
           ylab = "CO2 Concentration (Parts per Million)") # 연도별 서브셋 시계열 그래프

# 승법모델
AirPassengers
plot(AirPassengers, col = "maroon", lwd = 2, main = "Air Passengers", 
     xlab = "Year", ylab = "Air Passengers(1,000)")

lair <- log(AirPassengers)
plot(lair, col = "navy", lwd = 2, main = "Log Transformation Air Passengers", 
     xlab = "Year", ylab = "Air Passengers(log(1,000))") # 기법모델로 설명 가능해짐.

lair.decomp <- stl(lair, s.window = "periodic")
lair.decomp
plot(lair.decomp, col = "chocolate", col.range = "orange", lwd = 2,
     main = "국제선 항공기 승객수 시계열 분해(로그 변환)")

lair.decomp$time.series # 로그로 변환된 값
exp(lair.decomp$time.series) # 원래 값
