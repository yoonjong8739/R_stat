# 관측 가능한 여러 변수로부터 소수의 요인을 추출하여 이 요인들을 통해 변수 간의 관련성을 설명하려는 기법
# 요인적재값: 요인과 변수 간의 관계 (PCA 에서 성분적재값과 유사)
# 요인점수: 변수들의 선형결합으로 산출 (PCA 에서 성분점수와 유사)
# 측정 가능한 변수들로부터 그 안에 잠재되어 있는 해석 가능한 소수의 요인을 찾는 것이 목적

# 탐색적 요인분석: 요인을 사전에 알 수 없음. 분석 결과로서 도출. 
# 확인적 요인분석: 사전에 이론을 통해 제시된 요인이 있음. 데이터를 통해 이들 요인의 존재를 검정.SEM에서 측정 모델의 검증에 주로 사용.

# 공통 요인: 모든 변수는 이 두 요인의 함수로서 표현 가능
# 고유 요인: 공통 요인으로 설명되지 않는 부분. 변수의 고유한 특성으로서 남아 있는 성분과 오차.

# 요인 점수: 변수의 표준화 값 * 요인점수계수

# install.packages("ade4")
library(ade4)
data(olympic) # olympic 데이터는 1896년부터 2008년까지의 올림픽 메달리스트들의 신체적 특성
str(olympic)

# 1. 요인의 개수 결정
# 1) 1보다 큰 고유값을 갖는 요인 개수만큼 추출
# 2) Scree plot을 통해 고유값이 급격히 감소하는 지점에서 요인 개수 결정
# 3) Parallel analysis를 통해 고유값이 무작위 데이터 행렬로부터 추출된 고유값보다 큰 요인 개수만큼 추출
library(psych)
fa.parallel(olympic$tab, 
            fm = "ml",  # 요인 추출 방식: 최대우도법
            fa = "fa", n.iter = 100)

library(nFactors)
nScree(olympic$tab)
eigen(cor(olympic$tab)) # 고유값이 1보다 큰 요인 개수: 2개

# 2. 요인 분석 - factanal() 함수 사용
# factanal() 함수는 최대우도법을 사용하여 요인 분석을 수행
fa <- factanal(olympic$tab, factors = 2, 
    rotation = "varimax", # 요인 회전 방식: Varimax (기본값)
    scores = "regression") # 요인 점수 계산 방식: 회귀방식
fa
fa$loadings # 요인적재값
print(fa$loadings, cutoff = 0.001) # 요인적재값을 절대값 0.001 이상인 것만 출력

# 3. 평가
# 추출된 두 요인이 각 변수를 얼마나 잘 설명하는지 평가하깅 위해서는 공통요인에들에 의해 설명되는 각 변수의 분산 비율을 파악.
# 공통성: 각 변수의 분산 중 공통요인에 의해 설명되는 분산의 비율 (요인적재값의 제곱합)
# 공통요인에 의해 설명되지 않는 분산은 고유요인에 의해 설명되는 것으로 가정.
round(fa$uniqueness, 3) # 고유성: 1 - 공통성
round(1 - fa$uniqueness, 3) # 공통성: 1 - 고유성

# 실제 관측된 상관관계와 재현된 상관관계(요인적재값을 이용한 상관관계) 비교
round(fa$loadings %*% t(fa$loadings), 3) # 재현된 상관관계
round(cor(olympic$tab), 3) # 실제 관측된 상관관계

# 관측된 상관관계 - 재현된 상관관계 = 잔차 상관관계
round(cor(olympic$tab) - (fa$loadings %*% t(fa$loadings) + diag(fa$uniqueness)), 3)

# 4. 시각화(히트맵)
factor.plot(fa, labels = colnames(olympic$tab), pch = 20, pos = 4, title = "Factor Plot of Olympic Data")

library(gplots)
library(RColorBrewer)
heatmap.2(abs(fa$loadings), col = brewer.pal(9, "Blues"), trace = "none",
key = F, dendrogram = "none", cexCol = 1.2, main = "Factor Loadings")

# 4. 시각화(경로도): 요인과 개별 변수 간의 연결관계를 요인적재값과 함께 시각화
library(semPlot)
semPaths(fa, what="est",  # 경로상에 요인적재값을 나타냄
    residuals=F,     # 관측변수에 대한 잔차를 그래프상에 생략
    cut=0.3,         # 절대값으로 0.3보다 작은 요인적재값은 그래프상에 나타나지 않음
    posCol=c("white", "darkgreen"),  # 양의 요인적재값 가운데 0.3보다 작은 값은 흰색으로 표시
    negCol=c("white", "red"),        # 음의 요인적재값 가운데 절대값 0.3보다 작은 값은 흰색으로 표시
    edge.label.cex=0.75)             # 텍스트 크기

# 5. 요인점수
fa.scores <- fa$scores
fa.scores

colnames(fa.scores) <- c("Run", "Throw")
heatmap.2(fa.scores, col = brewer.pal(9, "GnBu"), trace = "none", key = F,
    dendrogram = "none", cexCol = 1.2, main = "Factor Scores by Athletes")

##########################################################################################
# 요인 분석 - fa() 함수 사용
# fa() 함수는 상관계수 행렬 또는 공분산 행렬을 입력으로 받아 요인 분석을 수행
# 데이터프레임 또는 행렬 형식의 데이터셋이 주어지면 상관계수 행렬이 자동으로 계산됨.
# 요인 점수 계산 방식: 회귀방식
library(psych)
fa <- fa(olympic$tab, 
    nfactors = 2,  # 추출할 요인 개수
    rotate = "varimax", # 요인 회전 방식: Varimax
    fm = "ml")  # 최대우도법
fa

fa$loadings # 요인적재값
fa$scores # 요인점수
fa$weights # 요인점수계수

fa.diagram(fa, 
    simple = F,  # 그래프상에 나타낼 요인적재값의 크기 지정 가능
    cut = 0.3,   # 최소 요인적재값: 절대값 0.3 이상인 것만 그래프상에 나타남
    digits = 2,  # 소수점 이하 자리수
    col = "blue", 
    adj = 2,  # 요인적재값의 위치 조정
    e.size = 0.08,  # 요인을 둘러쌀 타원형의 크기
    rsize = 2
)
