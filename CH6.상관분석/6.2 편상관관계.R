# 편상관관계
# 두 변수 간의 순수한 상관관계
colnames(mtcars)
mtcars2 <- mtcars[, c("mpg", "cyl", "hp", "wt")]
cor(mtcars2)

# 연비와 마력 간의 편상관계수 구하기
#install.packages("BiocManager")
BiocManager::install("graph")
#install.packages("ggm")
library(ggm)
pcor(c(1, 3, 2, 4), # 1,3:분석 대상 2,4:통제 변수 
     cov(mtcars2))  # 공분산 행렬

pcor(c("mpg", "hp", "cyl", "wt"), cov(mtcars2))

# 편상관관계 유의성 검정
# 귀무가설: 통제변수 cyl, wt를 제외한 mpg와 hp의 순수한 상관관계는 존재하지 않는다.
# 대립가설: 통제변수 cyl, wt를 제외한 mpg와 hp의 순수한 상관관계는 존재한다.
pcor.test(pcor(c(1,3,2,4), cov(mtcars2)), 
          q = 2, # 통제 변수 개수 
          n = nrow(mtcars2)) # 표본크기(관측값 개수)

#install.packages("ppcor")
# ggm 패키지의 함수명과 동일하므로 detach(package:ggm) 또는 detach(package:ppcor)을 사용
library(ppcor)
pcor(mtcars2)
pcor.test(mtcars2["mpg"], mtcars2["hp"], mtcars2[c("cyl", "wt")])
