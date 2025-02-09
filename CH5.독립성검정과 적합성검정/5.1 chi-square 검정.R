# chi-square 검정
# 교차표상의 응답 빈도를 바탕으로 수행.

# 독립성 검정: 두 범주형 변수 간의 관련성이 모집단에 존재하는지 검정. 2개 이상의 범주형 변수의 범주별 비율 분포 검정.
# 적합성 검정: 관측된 범주별 빈도를 바탕으로 모집단에서 기대되는 비율 분포가 존재하는지 검정.
survivors <- matrix(c(1443, 151, 47, 1781, 312, 135), ncol = 2)
dimnames(survivors) <- list("Status" = c("minor injury", "serious injury", "dead"),
                            "Seatbelt" = c("With Seatbelt", "Without Seatbelt"))
survivors

# 교차표에 행과 열의 합을 추가
addmargins(survivors)


# 교차표에 행과 열의 비율을 추가
addmargins(prop.table(addmargins(survivors,2),2),1)

# 안전벨트 착용과 안전 간의 관계를 막대도표로 시각화하기
barplot(survivors, ylim = c(0, 2500), las = 1, col = c("yellowgreen", "lightsalmon", "orangered"),
        ylab = "Frequency", main = "Frequency of Survivors")
legend(0.2, 2500, rownames(survivors), fill = c("yellowgreen", "lightsalmon", "orangered"))

survivors.prop <- prop.table(survivors, 2)
survivors.prop
barplot(survivors.prop*100, las = 1, col = c("yellowgreen", "lightsalmon", "orangered"),
        ylab = "Frequency", main = "Frequency of Survivors")

# 안전벨트 착용과 안전 간의 관련성 검정하기
# 귀무가설 : 안전벨트 착용과 승객 안전 간에는 연관이 없다.
# 대립가설 : 안전벨트 착용과 승객 안전 간에는 연관이 있다.

# 특정 카이제곱값(검정통계량)에 대응되는 유의확률 구하기
pchisq(45.91, df=2, lower.tail = F)

# 특정 확률에 대응되는 카이제곱값(검저옹계량) 산출
qchisq(0.05, df=2, lower.tail = F)
