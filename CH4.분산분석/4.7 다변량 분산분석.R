# 다변량 분산분석
# 2개 이상의 종속변수가 있을 경우 다변량 분산분석을 이용하여 집단별 차이를 동시에 검정 가능.
#install.packages("heplots")
library(heplots)
str(Skulls)

library(dplyr)
sample_n(Skulls, 10) # 데이터 무작위 10개 추출

# 시대에 따라 두개골 측정값이 다른지 검정
Skulls %>% 
  group_by(epoch) %>% 
  summarise(mb = mean(mb),
            bh = mean(bh),
            bl = mean(bl),
            nh = mean(nh)) %>% as.data.frame()

attach(Skulls)
y <- cbind(mb, bh, bl, nh)

# 귀무가설 : 시대에 따라 두개골 측정값은 차이가 없다.
# 대립가설 : 시대에 따라 두개골 측정값은 차이가 있다.(채택)
Skulls.manova <- manova(y ~ epoch)
summary(Skulls.manova)
summary.aov(Skulls.manova) # 일변량 분산분석 수행

# nh(코 높이)를 제외한 나머지 두개골 측정값은 시대별로 차이가 있음.
