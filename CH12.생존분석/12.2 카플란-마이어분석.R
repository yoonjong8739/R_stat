# 관측된 생존시간으로부터 생존확률을 추정
# 누적생존비율을 계산
# 생존함수를 산출하기 위해서 중도절단 데이터를 포함한 모든 생존시간이 오름차순으로 정렬.
# 중도절단 데이터 시간이 사건발생 데이터의 시간과 같으면 사건발생 데이터가 먼저 위치함.
# 중도절단 데이터가 있을 경우 생존비율은 1이 된다.

# 집단 간 생존함수 비교는 로그순위검정을 이용하여 통계적으로 검정 가능.
# 귀무가설: 모든 시점에서 집단 간에 생존시간의 차이가 없다.
# 대립가설: 모든 시점에서 집단 간에 생존시간의 차이가 있다.

library(survival)
library(survminer)
head(lung)
str(lung)
lung$sex <- factor(lung$sex, levels = c(1,2), labels = c("Male", "Female"))

# Surv 객체 생성
Surv(time = lung$time,    # 생존시간
     event = lung$status) # 사건발생 상태
class(Surv(time = lung$time, event = lung$status))

# 카플란-마이어 생존함수
# 전체 생존시간 데이터를 바탕으로 하나의 생존함수를 추정.
km.fit <- survfit(Surv(time = time, event = status) ~ 1, data = lung)
km.fit
names(km.fit)

km.df <- data.frame(time = km.fit$time, n.risk = km.fit$n.risk, n.event = km.fit$n.event,
                    n.censor = km.fit$n.censor, surv = km.fit$surv, upper = km.fit$upper, lower = km.fit$lower)
head(km.df)

# 생존함수곡선 시각화
plot(km.fit, xlab = "Days", ylab = "Overall Survival Probability")
plot(km.fit, xlab = "Days", ylab = "Overall Survival Probability", mark.time = T) # 중도절단 표시
ggsurvplot(fit = km.fit, xlab = "Days", ylab = "Overall Survival Probability")
ggsurvplot(fit = km.fit, xlab = "Days", ylab = "Overall Survival Probability", censor = F) # 중도절단 표시X
#ggsurvplot(fit = km.fit, data = lung, pval = T, risk.table = T, conf.int = T)

summary(km.fit, times = c(180, 360))  # 환자가 6개월 1년동안 생존할 확률
quantile(km.fit, probs = 1-c(0.7, 0.3))  # 환자의 생존확률이 70%, 30%일 때 경과된 일수
quantile(km.fit, probs = 0.5)  # 환자의 생존확률이 50%일 때 경과된 일수

# 집단 간 생존시간 차이 비교
km.group <- survfit(Surv(time = time, event = status) ~ sex, data = lung)
km.group
summary(km.group)
summary(km.group)$table
summary(km.group, times = c(180, 360))

km.summary <- surv_summary(km.group, data = lung)
km.summary
attr(km.summary, "table") # summary(km.group)$table 과 동일

# 로그순위검정(멘텔-헨젤검정): 집단 간 생존함수 비교
# 귀무가설: 성별에 따른 폐암 환자의 생존시간 차이가 없다.
# 대립가설: 성별에 따른 폐암 환자의 생존시간 차이가 있다.(채택)
survdiff(Surv(time = time, event = status) ~ sex, data = lung)

# 집단별 생존함수 시각화
ggsurvplot(km.group, pval = T,          # 로그순위검정 결과 유의확률 표시
           conf.int = T,                # 생존함수 95% 신뢰구간
           risk.table = "abs_pct",      # 현재 시점까지 남아있는 생존자 수 및 비율. absolute, percentage, abs_pct 지정
           risk.table.col = "strata",   # 그룹별 risk table 색상 맞추기 
           linetype = "strata",         # 그룹별 선 종류 다르게 
           surv.median.line = "hv",     # 수평선/수직선
           ggtheme = theme_bw(), 
           palette = c("royalblue", "salmon"))

ggsurvplot(km.group, pval = T,          
           conf.int = T,                
           conf.int.style = "step",     # 신뢰구간 영역 표시 스타일 지정. ribbon, step
           xlab = "Days", break.time.by = 180,
           risk.table = "abs_pct",      
           risk.table.fontsize = 3.5, risk.table.y.text = F,
           risk.table.col = "strata",   
           linetype = "strata",         
           ncensor.plot = T,  # 중도절단표 생성. 각 생존시간에서의 중도절단 데이터 개수 표시
           surv.median.line = "hv",    
           legend.labs = c("Male", "Female"), legend.title = "",
           ggtheme = theme_light(), 
           palette = c("royalblue", "salmon"))

# 연구기간 끝 부분에 추적이 불가능하거나 생존 상태인 경우가 존재하기 때문에 일부 기간으로 제한하여 그래프 범위를 축소하는 것이 바람직할 수 있음.
ggsurvplot(km.group, pval = T, conf.int = T, risk.table.col = "strata",   
           linetype = "strata", ggtheme = theme_bw(), palette = c("royalblue", "salmon"),
           xlim = c(0, 600))

# 누적사건곡선 생성, 1-생존함수
# 생존함수곡선과 반대로 특정 시점까지 사망할 확률을 볼 수 있음.
ggsurvplot(km.group, pval = T, conf.int = T, risk.table.col = "strata",   
           linetype = "strata", ggtheme = theme_bw(), palette = c("royalblue", "salmon"),
           fun = "event")

# 누적위험함수곡선 생성
# 특정 시점까지 누적사망률
# ex) 게임 캐릭터의 1시간 동안 누적사망률이 3이라는 것은 캐릭터가 죽을 때마다 즉각 부활시켜 게임을 진행할 때 1시간 동안 그 캐릭터가 3번 죽을 것으로 기대한다는 것을 의미.
ggsurvplot(km.group, pval = T, conf.int = T, risk.table.col = "strata",   
           linetype = "strata", ggtheme = theme_bw(), palette = c("royalblue", "salmon"),
           fun = "cumhaz")

# 다중집단 카플란-마이어분석
head(colon) # 대장암 환자 데이터
str(colon)
colon.death <- colon[colon$etype==2, ]  # etype=1:재발, etype=2:사망
colon.death$sex <- factor(colon.death$sex, levels = c(0,1), labels = c("Female", "Male"))
colon.death$differ <- factor(colon.death$differ, levels = c(1,2,3), labels = c("Well", "Moderate", "Poor"))

# 성별, 치료법, 종양분화상태의 조합에 따른 생존함수 추정
km.fit <- survfit(Surv(time = time, event = status) ~ sex + rx + differ, data = colon.death)
km.fit
summary(km.fit)
surv_summary(km.fit)

# 치료법과 조양본화상태 범주 조합에 따른 남녀별 생존함수곡선
ggsurvplot(km.fit, conf.int = T, conf.int.style = "step", ggtheme = theme_bw())$plot +
  theme_bw() +
  theme(legend.position = "right", legend.title = element_blank()) +
  facet_grid(rx ~ differ, labeller = label_both)



