# 독립성검정
# 두 범주형 변수가 서로 독립인지 검정
str(Titanic)
Titanic

# 2차원 형태의 교차표로 데이터를 변환
Titanic.margin <- margin.table(Titanic, margin = c(4, 1)) # Survived와 Class의 교차표
Titanic.margin

# 교차표에 행과 열의 합 추가
addmargins(Titanic.margin)
addmargins(Titanic.margin, margin = 2)

prop.table(addmargins(Titanic.margin, margin = 2), margin = 2)
addmargins(prop.table(addmargins(Titanic.margin, margin = 2), margin = 2), margin = 1)

# 카이제곱 독립성 검정
# 귀무가설: 승객 구분에 따른 생존율의 차이가 없다.
# 대립가설: 승객 구분에 따른 생존율의 차이가 있다.(채택)
chisq.test(Titanic.margin)

# 대립가설 채택 -> 과련성의 강도를 평가
#install.packages("vcd")
library(vcd)
assocstats(Titanic.margin) # 값이 클수록 두 변수 간의 관련성이 크다는 것을 의미
mosaic(Titanic.margin, shade = T, legend = T)
mosaic(~ Survived + Class, data = Titanic.margin, shade = T, legend = T)


# 데이터프레임으로 저장된 데이터셋은 교차표 생성 필요 없음
library(MASS)
str(survey)

# 귀무가설: 성별에 따른 팔짱 손 위치에 차이가 없다.(채택)
# 대립가설: 성별에 따른 팔짱 손 위치에 차이가 있다.
with(survey, chisq.test(Sex, Fold))

# 교차표 생성 후 실행해도 상관없음
crosstab <- with(survey, table(Sex, Fold))
crosstab
chisq.test(crosstab)
