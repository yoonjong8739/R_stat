# 추세성분, 계정성분, 불규칙 성분 등으로 구성
# 지수모델링: 가중평균 사용
# ARIMA: 관측값과 오차 간의 상관

# 보스턴 가구당 월별 에너지 소비 데이터
url <- "http://jse.amstat.org/datasets/utility.dat.txt"
utility <- read.table(url, stringsAsFactors = F)
utility
utility.ts <- ts(utility[[7]],  # 전기 사용량
                 start = c(1990, 9), 
                 frequency = 12)  # 월별 데이터 (연도: 1, 6개월: 2, 분기: 4, 일별: 7)
utility.ts
plot(utility.ts)

start(utility.ts)
end(utility.ts)

frequency(utility.ts) # 단위 시간에 포함된 관측값 개수

deltat(utility.ts) # 관측값 간의 시간 간격(1/12)

time(utility.ts) # 각 관측값이 추출되는 시점을 시계열값으로 반환
cycle(utility.ts) # 각 관측값에 대응되는 주기의 일련번호

window(utility.ts, start = c(1991, 1), end = c(1992, 6)) # 시계열 데이터셋의 서브셋 생성
window(utility.ts, start = c(1991, 1), frequency = 1) # 1월 관측값만 출력
window(utility.ts, start = c(1991, 7), frequency = 1) # 7월 관측값만 출력
window(utility.ts, start = c(1990, 9), frequency = 2) # 6개월 간격으로 관측값을 추출
window(utility.ts, start = c(1991, 1), frequency = 4) # 분기 간격으로 관측값을 추출
