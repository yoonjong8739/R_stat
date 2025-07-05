# 서로 상관관계를 갖는 많은 변수를 상관관계가 없는 소수의 변수로 변환하는 차원축소 기법
# 변수들이 갖고 있는 총 표본분산을 많이 설명해주는 순서대로 순차적으로 변수 개수만큼의 성분을 추출
str(state.x77)
colnames(state.x77)
pca <- prcomp(state.x77, scale = TRUE)
summary(pca)
plot(pca, type='l', pch=19, lwd=2, col='red', main='Scree Plot')

# 성분적재값: 성분과 변수 간의 상관 정도. 값이 클수록 성분과 변수 간 관련성이 높음.
round(pca$rotation, 3)

# 성분점수: 변수값(표준화)에 성분적재값을 곱하고 이를 합산
round(scale(state.x77) %*% pca$rotation, 3) # 표준화된 변수값에 성분적재값을 곱한 것
round(pca$x, 3) # 성분점수
round(pca$x[, c(1, 2)], 3) # 설명력이 큰 상위 2개 성분점수 추출

# 주성분분석은 변수들이 갖고 있는 총분산을 많이 설명해주는 순서대로 순차적으로 성분을 추출
# 동시에 이들 성분이 서로 상관관계를 갖지 않도록 한다.
round(cor(pca$x), 3)
biplot(pca, cex=c(0.5, 0.75), col=c("blue", "red"),
    main="Biplot of PCA on state.x77") # 화살표가 축과 평행일수록 대응되는 변수와 성분이 서로 밀접한 상관관계를 가짐.
