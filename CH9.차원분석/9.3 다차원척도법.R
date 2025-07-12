# 다차원척도법(MDS)
# 케이스 간의 거리를 바탕으로 이들 간의 관계 구조를 시각적으로 표현하는 통계데이터분석 기법
str(eurodist)
labels(eurodist)
as.matrix(eurodist)[1:5, 1:5]

eurocity_mds <- cmdscale(d = eurodist) # 다차원척도법
head(eurocity_mds)

plot(eurocity_mds, type = "n", main = "Multidimensional Scaling Plot")
text(eurocity_mds, rownames(eurocity_mds), col = "maroon", cex = 0.7)

str(USJudgeRatings)
USJudgeRatings_dist <- dist(USJudgeRatings) # 유클리드 거리 계산
USJudgeRatings_mds <- cmdscale(USJudgeRatings_dist)
plot(USJudgeRatings_mds, type = "n", main = "Multidimensional Scaling Plot")
text(USJudgeRatings_mds, rownames(USJudgeRatings_mds), col = "blue", cex = 0.7)

str(mtcars)
library(cluster)
mtcars_dist <- daisy(mtcars, metric = "gower") # 계량/비계량 척도가 혼재된 데이터의 거리 계산

library(MASS)
mtcars_mds <- isoMDS(mtcars_dist) # 비계량 데이터로 계산한 거리행렬에 대해 다차원척도법 수행
str(mtcars_mds)
plot(mtcars_mds$points, type = "n", main = "Multidimensional Scaling Plot")
text(mtcars_mds$points, rownames(mtcars), col = "purple", cex = 0.7)
