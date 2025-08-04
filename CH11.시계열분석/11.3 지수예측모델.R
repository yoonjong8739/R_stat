# 1. 단순지수평활법
# 예측 모델 생성 시 불규칙 성분만 고려되며 추세와 계절 요인은 포함되지 않음.
# 데이터에 명확한 **트렌드(상승/하락)나 계절성(주기적 패턴)**이 없을 때 사용.
# 수준을 추정
# 트렌드나 계절성이 없는 단순한 시계열 데이터에 적합한 예측 방법
# 과거 데이터의 가중 평균을 사용해 미래 값을 예측
# 최근 데이터에 더 큰 가중치를 주고, 오래된 데이터에는 점점 적은 가중치를 적용

# 2. 홀트지수평활법
# 추세를 갖는 시계열 예측모델을 생성.
# 단순지수평활법을 확장해 **트렌드(상승 또는 하락 경향)**가 있는 시계열 데이터를 처리
# 데이터에 트렌드가 있지만 계절성은 없을 때.
# 수준과 기울기 추정
# 예: 꾸준히 증가하는 웹사이트 방문자 수.

# 3. 홀트-윈터지수평활법
# 데이터의 수준, 트렌드, 계절적 패턴(예: 매년 여름 매출 증가)을 모두 고려해 예측
# 수준, 기울기, 계절 요인이 모두 포함된 시계열 예측
# 예: 계절마다 매출이 달라지는 소매점(여름에 매출 증가, 겨울에 감소).

LakeHuron # 1875년 ~ 1972년까지 98년간 매년 측정한 호수 수위 시계열 데이터

# 단순지수평활법
library(forecast)
lake.ets <- ets(LakeHuron, model = "ANN") # A:불규칙 성분이 가법적, NN: 추세, 계절 성분 존재X
lake.ets

lake.ets.pred <- forecast(lake.ets, h = 1) # 1년 후의 호수 수위 예측
lake.ets.pred

plot(lake.ets.pred, col = "royalblue", lwd = 2,
     main = "Forecast for Annual Level of Lake Huron", xlab = "Year", ylab = "Level (Feet)")
accuracy(lake.ets)

# 홀트지수평활법
#library(fpp3)
library(fpp2)
elecsales # 1989년 ~ 2008년

library(forecast)
elecsales.ets <- ets(elecsales, model = "AAN") # AA: 불규칙 성분(가법적) & 트렌드(가법적), N: 계절 성분 존재X 
elecsales.ets
accuracy(elecsales.ets)

elecsales.ets.pred <- forecast(elecsales.ets, h = 5) # 5년간의 예측 전력 판매량
elecsales.ets.pred

plot(elecsales.ets.pred, col = "royalblue", lwd = 2,
     flty = 3, flwd = 3, shadecols = c("lavender", "mistyrose"), 
     main = "Forecast for Electricity Sales in South Africa", xlab = "Year", ylab = "Electricity Sales (Gwh)")

# 홀트-윈터지수평활법
library(forecast)
lair.ets <- ets(log(AirPassengers), model = "AAA")
lair.ets
accuracy(lair.ets)

lair.ets.pred <- forecast(lair.ets, h = 12) # 12개월(1년) 후 예측
lair.ets.pred

plot(lair.ets.pred, col = "salmon", lwd = 2, fcol = "indianred1", flwd = 3,
     main = "Forecast for Air Passengers", xlab = "Year", ylab = "Air Passengers (Log(Thousand Persons))")

air.mean <- exp(lair.ets.pred$mean)
air.lower <- exp(lair.ets.pred$lower)
air.upper <- exp(lair.ets.pred$upper)
air.pred <- cbind(air.mean, air.lower, air.upper)
air.pred

# 완화추세와 모델자동선택
# damped = T : 완화추세 적용
# model 파라미터 지정하지 않으면 자동으로 가장 우수한 모델을 선택

austourists # 호주를 방문한 여행객의 체류기간을 분기별로 기록
austourists.ets <- ets(austourists)
austourists.ets

plot(forecast(austourists.ets, h = 12),
     col = "cornflowerblue", lwd = 2, flty = 1, flwd = 3, fcol = "royalblue", 
     shadecols = c("mistyrose", "salmon"), main = "Forecast for International Tourists to Australia",
     xlab = "Year", ylab = "Total Visitor Nights")



