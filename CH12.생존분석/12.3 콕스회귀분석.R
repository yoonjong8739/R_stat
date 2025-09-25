# 콕스 회귀분석 : 생존시간과 하나 이상의 예측변수 간의 관계 분석. 범주형/연속형 변수 고려
# 일반적인 다중회귀분석처럼 복수의 요인을 동시에 투입라여 다른 예측변수들이 일정하다는 가정하에 각 예측변수가 사건발생률(ex, 사망률)에 미치는 영향을 분석석
# 카플란-마이어분석: 일변량분석, 생존시간에 영향을 미치는 다른 변수는 고려하지 않음. 예측변수가 범주형 변수일 때만 유용.

# 위험률 대비 실제 위험률 비에 자연로그를 취한 값 도출.
# 위험비 : (연속형 변수)예측변수 한 단위 증가 시 변화하는 위험률. (범주형 변수)기준범주 대비 상대위험도.

# 비례위험가정 : 위험비가 생존기간 내내 일정하다고 가정.
# 만일, 연구기간 동안 집단 간 위험비가 일정하지 않으면 다른 분석방법을 고려해야 함.
# 콕스모델 추정 후에는 반드시 비례위험가정 여부를 확인해야 함.

# 카플러-마이어분석의 로그순위검정 또한 비례위험가정을 필요로 함.

library(survival)
lung <- survival::lung
head(lung)
str(lung)
lung$sex <- factor(lung$sex, levels = c(1,2), labels = c("Male", "Female"))
head(lung)

# 콕스회귀모델
cox <- coxph(Surv(time = time, event = status) ~ age + sex + ph.ecog, data = lung)
cox
summary(cox)

# 결과해석
# 1. 회귀계수의 통계적 유의성 : z열(왈드통계량, 회귀계수/표준오차). sex, ph.ecog 변수는 통계적으로 유의
# 2. 회귀계수의 부호 : (+) 부호는 변수값이 증가할수록 위험률이 증가. (-)부호는 위험률이 감소. age, ph.ecog 변수는 나이와 일상생활 점수가 증가할수록 위험률은 증대. 반면에 여성인 경우 남성인 경우보다 위험률이 감소.
# 3. 위험비 : sex(다른 변수들이 일정하다는 가정하에 여성은 남성보다 사망할 가능성이 42.5% 낮음), age(나이 1살 증가는 사망확률 1.011배 증가, 1.1% 증가), ph.ecog(ECOG 점수가 1점 증가는 사망확률 1.59배 증가, 59% 증가)
# 4. 위험지 신뢰구간 : 신뢰구간(95%) 내에 1이 포함되지 않으면 위험비는 유의수준 0.05에서 통계적으로 유의하다.
# 5. 전체 모델의 유의성 : (귀무가설)모든 회귀계수가 0이다. (대립가설)모든 회귀계수는 0이 아니다. 모두 귀무가설을 기각하므로 모델은 유의하다.

# 우도비검정(Likelihood ratio test) - 작은 표본에도 우수. 일반적으로 선호
# 왈드검정(Wald test)
# 로그순위검정(Score (logrank) test)

# 콕스회귀분석 결과 시각화
# 예측변수의 위험비와 유의성 확인 가능
library(survminer)
ggforest(model = cox, data = lung)

# 생존함수 추정
cox.fit <- survfit(cox, data = lung) 
cox.fit

# 생존함수곡선
# 예측변수들이 평균값일 때의 생존확률 추정
ggsurvplot(cox.fit, palette = "cornflowerblue", ggtheme = theme_minimal(),
           legend = "none", xlab = "Days", ylab = "Overall Survival Probability")

# 성별이 생존확률에 미치는 영향
# 여성이 남성에 비해 생존확률이 더 크다.
sex.df <- data.frame(sex = c("Male", "Female"),
                     age = rep(mean(lung$age, na.rm = T), 2),
                     ph.ecog = rep(mean(lung$ph.ecog, na.rm = T), 2))
sex.df

sex.fit <- survfit(cox, newdata = sex.df, data = lung)
ggsurvplot(sex.fit, conf.int = F, ggtheme = theme_minimal(),
           legend.labs = c("Male", "Female"), legend.title = "", xlab = "Days", ylab = "Survival Probability")

# 일상생활능력이 생존확률에 미치는 영향
# 일상생활이 양호할수록 생존확률은 커짐.
ph.df = data.frame(sex = rep("Male", 4),
                   age = rep(mean(lung$age, na.rm = T), 4),
                   ph.ecog = c(0, 1, 2, 3))
ph.df

ph.fit <- survfit(cox, newdata = ph.df, data = lung)
ggsurvplot(ph.fit, conf.int = F, ggtheme = theme_minimal(),
           legend.labs = c(0:3), legend.title = "ECOG Performance Score (0=good)", 
           xlab = "Days", ylab = "Survival Probability")

# 비례위험가정 위반 여부 검토
# 잔차는 시간에 독립적
# 시간이 경과함에 따라 예측변수와 잔차 간에 규칙적인 패턴이 관찰되면 비례위험가정을 위반
cox.test <- cox.zph(cox)
cox.test

# 각 예측변수별로 잔차와 시간 간의 상관관계를 추정하여 이들 간의 독립성을 검정
# 잔차와 시간 간의 상관관계가 유의하면 비례위험가정 위반
ggcoxzph(cox.test)
