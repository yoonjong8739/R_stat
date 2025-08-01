# 분할적 군집분석은 계층적 군집분석과는 달리, 도출하고자 하는 군집의 수를 사전에 결정

# k-평균 군집분석
# 1. 개별 케이스와 k개 중심점과의 거리를 측정하여 가장 가까운 거리를 갖는 군집에 개별 케이스를 할당하는 방식
# 2. 더 큰 규모의 데이터셋을 다룰 수 있음.
# 3. 영원히 하나의 군집에 귀속되지 않고 최적의 군집을 찾는 과정에서 매 단계마다 업데이트
# 4. 모든 변수가 연속형이어야 함.
# 5. 이상점에 의해 영향을 받을 수 있음.

head(state.x77)
state.scaled <- scale(state.x77)

library(NbClust)
set.seed(123)
nc <- NbClust(data = state.scaled, distance = "euclidean",
              min.nc = 2, max.nc = 15, method = "kmeans")
table(nc$Best.nc[1,])
barplot(table(nc$Best.nc[1,]), col = "lightblue",
        xlab = "Number of Cluster", ylab = "Number of Supporting Index",
        main = "Number of Clusters Proposed by Indices")

set.seed(123)
clustering.km <- kmeans(state.scaled, centers = 3, nstart = 25)  # 25번 반복
clustering.km$cluster
clustering.km$size
aggregate(state.x77, by = list(cluster = clustering.km$cluster), mean)  # 군집별 평균

library(cluster)
clusplot(x = state.x77, clus = clustering.km$cluster, 
         color = T, shade = T, labels = 2, lines = 0, main = "Cluster Plot")

library(factoextra)
fviz_cluster(clustering.km, data = state.x77, ellipse = T, geom = "point")

# PAM 군집분석
# 1. 이상치에 취약한 k-means clustering을 보완.
# 2. 평균 대신 실제 관측값 중 하나를 군집을 대표하는 중심점으로 사용.
# 3. 여러 유형이 혼재된 변수에 대해서도 수행 가능.
# 4. 무작위로 k개의 캐이스를 선택하고 시작. 이후 모든 케이스와 각 메도이드 간의 거리를 계산하여 케이스들을 가장 가까운 메도이드 군집에 할당. 중심점 메도이드와 케이스들 간의 거리 합을 군집의 총비용이라고 함.
# 5. 계산된 총비용이 기존의 총비용보다 작으면 새로운 케이스가 메도이드로 지정.
# 6. 더 이상 업데이트가 되지 않을 때까지 반복 수행.

library(rattle)
head(wine)

library(cluster)
set.seed(123)
clustering.pam <- pam(wine[-1], k=3, stand = T)  # 거리행렬 데이터일 경우 stand 인수는 무시됨.
clustering.pam$clusinfo  # 군집에 대한 요약 정보
clustering.pam$medoids  # 각 군집을 대표하는 케이스에 대한 정보
clustering.pam$id.med  # 메도이드의 케이스 번호(3개 군집의 메도이드 번호)
clustering.pam$clustering  # 각 케이스별 소속 군집

aggregate(wine[-1], by = list(cluster = clustering.pam$clustering), mean)

clusplot(clustering.pam, color = T, shade = T, labels = 4, # 군집 번호만 표시
         lines = 0, main = "Cluster Plot")
fviz_cluster(clustering.pam, data = wine[-1], ellipse = T, geom = "point")

result.pam <- table(wine$Type, clustering.pam$clustering, dnn = c("Actual", "Clusterd"))
result.pam
mean(wine$Type == clustering.pam$clustering)  # 분류 정확도

# 실제 와인 유형과 PAM 군집분석에 의한 와인 유형 간 일치 정도
library(flexclust)
randIndex(result.pam)  # -1 ~ 1 사이
