# 계층적 군집분석은 모든 케이스가 각자 하나의 군집을 형성하면서 시작
# 군집 간의 거리: 그 안에 포함된 케이스 간의 거리

# 거리 측정 방법
# 1) 단일 연결법: 두 군집의 모든 케이스쌍의 거리 중 가장 가까운 거리를 사용.
# 2) 완전 연결법: 두 군집의 모든 케이스쌍의 거리 중 가장 먼 거리를 사용.
# 3) 평균 연결법: 두 군집의 모든 케이스쌍의 거리를 평균한 거리를 사용.
# 4) 중심 연결법: 두 군집의 중심 간 거리를 사용. 군집의 중심은 변수값 평균을 이용.
# 5) 최소분산연결법: 두 군집 내 모든 케이스 간의 총분산을 거리로 사용. 총분산이 최소화되는 군집들을 결합.

library(flexclust)
data("nutrient")
nutrition <- nutrient
row.names(nutrition) <- tolower(row.names(nutrition))
nutrition.scaled <- scale(nutrition)

d <- dist(nutrition.scaled)  # 기본값: 유클리디안 거리
clustering.average <- hclust(d, method = "average")  # 평균 연결법

# 덴드로그램
plot(clustering.average, hang = -1, cex = 0.9, col = "darkgreen",
     xlab = "Food", main = "계층적 군집분석(평균연결법)")

# NbClust 패키지의 NbClust() 함수는 최적의 군집 개수를 알려주는 다양한 지표를 제공
library(NbClust)
nc <- NbClust(data = nutrition.scaled, distance = "euclidean", min.nc = 3, max.nc = 15, method = "average")
nc$Best.nc
table(nc$Best.nc[1,])  # Number_clusters
barplot(table(nc$Best.nc[1,]),
        xlab = "Number of Cluster", ylab = "Number of Supporting Index",
        main = "Number of Clusters Proposed by Indices")

clusters <- cutree(clustering.average, k = 5)
clusters
table(clusters)

# 덴드로그램과 군집
plot(clustering.average, hang = -1, cex = 0.9, col = "darkgreen",
     xlab = "Food", main = "계층적 군집분석(평균연결법)")
rect.hclust(clustering.average, k = 5)

# 5개 군집의 영양 성분별 평균
aggregate(nutrition, by = list(clusters), mean)

a <- aggregate(nutrition.scaled, by = list(cluster = clusters), mean)
n <- as.vector(table(clusters))  # 각 군집별 할당된 케이스 개수
cbind(a, n)

# 계층적 군집분석운 의미있는 계층적 구조를 기대할 수 있을 때 유용.
# 그러나, 특정 군집으로 할당되면 더이상 바뀌지 않음.
# 케이스 수가 많을 때 거리를 계산한 행렬이 커지게 되므로 대용량 데이터에 적용하기에는 적합하지 않음.