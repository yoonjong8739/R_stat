library(MASS)
str(survey)

frqtab <- table(survey$Smoke) # Smoke 레벨별 빈도
frqtab

class(frqtab)
frqtab[2]  # 인덱스나 이름을 이용하여 특정 원소를 선택할 수 있음.

# 최빈값
frqtab==max(frqtab)
frqtab[frqtab==max(frqtab)]
names(frqtab[frqtab==max(frqtab)])

which.max(frqtab) # 최댓값 위치 인덱스 반환
frqtab[which.max(frqtab)]
names(frqtab[which.max(frqtab)])

# 비율 구하기
frqtab.prob <- prop.table(frqtab)
frqtab.prob
frqtab.prob["Never"]
frqtab.prob*100

mean(survey$Smoke=="Never", na.rm = T)

mean(anorexia$Postwt > anorexia$Prewt) # 치료후 몸무게가 증가한 환자의 비율
mean(abs(mammals$brain-mean(mammals$brain)) > 2*sd(mammals$brain)) # 두뇌의 무게가 평균으로부터 2표준편차보다 큰 동물의 비율
mean(diff(SP500) > 0) # S&P500지수 수익률이 전일보다 증가한 일자의 비율

# 교차표 만들기
#install.packages("vcd")
library(vcd)
str(Arthritis)

crosstab <- table(Arthritis$Improved, Arthritis$Treatment) # dnn 인수에 행/열이름 지정 가능
crosstab
crosstab["Marked", "Treated"]

crosstab <- xtabs(~ Improved + Treatment, data = Arthritis)
crosstab

margin.table(crosstab, margin = 1) # 행 기준 빈도 합
prop.table(crosstab, margin = 1) # 행 기준 비율

margin.table(crosstab, margin = 2) # 열 기준 빈도 합
prop.table(crosstab, margin = 2) # 열 기준 비율

prop.table(crosstab) # 개별 셀의 비율

# 빈도합을 포함한 교차표
addmargins(crosstab, margin = 1) # 각 열의 합을 구한 새로운 행을 추가
addmargins(crosstab, margin = 2) # 각 행의 합을 구한 새로운 열을 추가
addmargins(crosstab) # 두 변수 모두에 대한 합의 행과 열을 생성

addmargins(prop.table(crosstab, margin = 2), margin = 1)
addmargins(prop.table(crosstab, margin = 1), margin = 2)

#install.packages("gmodels")
library(gmodels)
CrossTable(Arthritis$Improved, Arthritis$Treatment, prop.chisq = F, dnn = c("Improved", "Treatment"))

# 다차원 테이블 생성
multab <- with(Arthritis, table(Improved, Sex, Treatment))
multab

multab <- xtabs(~Improved + Sex + Treatment, data = Arthritis)
multab

ftable(multab)
ftable(multab, row.vars = c(2,3)) # Improved, Sex -> Sex, Treatment
ftable(Arthritis[c("Improved", "Sex", "Treatment")], row.vars = c(2,3))

margin.table(multab, 1) # Improved 빈도수
margin.table(multab, 2) # Sex 빈도수
margin.table(multab, 3) # Treatment 빈도수
margin.table(multab, c(1,3)) # Improved X Treatment 교차표(인덱스로 주어지지 않은 Sex의 합)

ftable(prop.table(multab, c(2,3))) # Sex X Treatment 교차표의 Improved 비율
ftable(addmargins(prop.table(multab, c(2,3)), 1))
