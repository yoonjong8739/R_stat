# 3.4 대응표본 평균검정
# 검정하려고 하는 2개의 표본이 서로 독립이 아닌 모집단으로부터 추출했을 때 적용.

# 10명의 실험 대상자들에게 수면제 복용 전과 후 수면시간 비교
# 귀무가설 : 수면제 복용 전과 후의 차이가 없다.
# 대립가설 : 수면제 복용 전과 후의 차이가 있다.(채택)
str(sleep)
sleep[seq(1,20,2), ]

# 두 벡터로 데이터 추출
x <- sleep$extra[sleep$group==1] # subset(sleep, group==1)$extra
y <- sleep$extra[sleep$group==2] # subset(sleep, group==2)$extra

# 대응표본 t-검정
t.test(x, y, paired = TRUE)

## t.test() 함수에 포뮬러 형식으로 인수를 저장하기 위해서는 long format 형태의 데이터가 필요함. ##
# wide format의 형태로 되어 있으면 포뮬러를 이용하여 인수를 지정하지 못한다. ##
library(tidyr)
sleep.wide <- spread(data = sleep, key = group, value = extra) # pivot_wider() 사용 가능
sleep.wide

t.test(sleep.wide$'1', sleep.wide$'2', paired=T)