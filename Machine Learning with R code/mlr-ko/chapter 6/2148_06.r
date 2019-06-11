##### 6장 : 회귀(regression) 기법 -------------------

#### Part 1: 선형 회귀 -------------------

## 회귀 이해 ----
## 예제 : 스페이스 셔틀 발사 데이터 ----
launch <- read.csv("challenger.csv")

# 수동으로 베타 추정
b <- cov(launch$temperature, launch$distress_ct) / var(launch$temperature)
b

# 수동으로 알파 추정
a <- mean(launch$distress_ct) - b * mean(launch$temperature)
a

# 발사 데이터의 상관 관계 계산
r <- cov(launch$temperature, launch$distress_ct) /
       (sd(launch$temperature) * sd(launch$distress_ct))
r
cor(launch$temperature, launch$distress_ct)

# 상관 관계을 활용한 기울기 계산
r * (sd(launch$distress_ct) / sd(launch$temperature))

# lm 함수를 활용한 회귀 직선 확인(책에서 다루지 않음)
model <- lm(distress_ct ~ temperature, data = launch)
model
summary(model)

# 다중 회귀 함수 생성
reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  solve(t(x) %*% x) %*% t(x) %*% y
}

# 발사 데이터 확인
str(launch)

# 단순 선형 회귀인 회귀 모델 테스트
reg(y = launch$distress_ct, x = launch[3])

# 다중 회귀인 회귀 모델 사용
reg(y = launch$distress_ct, x = launch[3:5])

# lm 함수 사용하여 다중 회귀 결과 확인 (책에서 다루지 않음)
model <- lm(distress_ct ~ temperature + pressure + launch_id, data = launch)
model

## 예제 : 의료 비용 예측 ----
## 2단계 : 데이터 준비와 살펴보기 ----
insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)

# 비용 변수 요약
summary(insurance$charges)

# 보험 비용의 히스토그램
hist(insurance$charges)

# 지역 테이블
table(insurance$region)

# 속성간의 관계 살펴보기 : 상관 관계 매트릭스
cor(insurance[c("age", "bmi", "children", "charges")])

# 속성간의 관계 시각확 : scatterplot 매트릭스
pairs(insurance[c("age", "bmi", "children", "charges")])

# 좀 더 정보적인 scatterplot 매트릭스
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "charges")])

##3 단계 : 데이터로 모델 훈련 ----
ins_model <- lm(charges ~ age + children + bmi + sex + smoker + region,
                data = insurance)
ins_model <- lm(charges ~ ., data = insurance) # 위와 같음

# 추정한 베타 계수 확인
ins_model

## 4 단계 : 모델 성능 평가 ----
# 추정한 베타 계수 확인
summary(ins_model)

## 5 단계 :모델 성능 향상 ----

# 고차원 "age"항 추가
insurance$age2 <- insurance$age^2

# BMI >= 30에 대한 지시자 추가
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

# 최종 모델 생성
ins_model2 <- lm(charges ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance)

summary(ins_model2)

#### Part 2: 회귀 트리와 모델 트리 -------------------

## 회귀 트리와 모델 트리 이해 ----
## 예제 : SDR 계산 ----
# 데이터 준비
tee <- c(1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 7, 7)
at1 <- c(1, 1, 1, 2, 2, 3, 4, 5, 5)
at2 <- c(6, 6, 7, 7, 7, 7)
bt1 <- c(1, 1, 1, 2, 2, 3, 4)
bt2 <- c(5, 5, 6, 6, 7, 7, 7, 7)

# SDR 계산
sdr_a <- sd(tee) - (length(at1) / length(tee) * sd(at1) + length(at2) / length(tee) * sd(at2))
sdr_b <- sd(tee) - (length(bt1) / length(tee) * sd(bt1) + length(bt2) / length(tee) * sd(bt2))

# 각 구분에 대한 SDR 비교
sdr_a
sdr_b

## 예제 : 와인 품질 추정 ----
## 단계 2: 데이터 준비와 살펴보기 ----
wine <- read.csv("whitewines.csv")

# 와인 데이터 확인
str(wine)

# 품질 등급 분포
hist(wine$quality)

# 와인 데이터의 요약 통계
summary(wine)

wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

## 3 단계 : 데이터에 대한 모델 훈련 ----
# rpart를 활용한 회귀 트리 
library(rpart)
m.rpart <- rpart(quality ~ ., data = wine_train)

# 트리에 대한 기본 정보
m.rpart

# 트리에 대한 세부 정보
summary(m.rpart)

# 시각화 하기 위해 rpart.plot 패키지 사용
library(rpart.plot)

# 기본 결정 트리 다이어그램
rpart.plot(m.rpart, digits = 3)

# 일부 조정 다이어그램
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

## 4단계 : 모델 성능 평가 ----

# 테스트 데이터셋에 대해 예측치 생성
p.rpart <- predict(m.rpart, wine_test)

# 실제값과 예측값의 분포 비교
summary(p.rpart)
summary(wine_test$quality)

# 상관 관계 비교
cor(p.rpart, wine_test$quality)

# 평균 절대 오차(mean absolute error)를 계산하는 함수
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}

# 예측값과 실제값간의 평균 절대 오차
MAE(p.rpart, wine_test$quality)

# 예측값과 실제값간의 평균 절대 오차
mean(wine_train$quality) # result = 5.87
mean_abserror(5.87, wine_test$quality)

## 5단계 : 모델 성능 향상 ----
# M5' 모델 트리 훈련
library(RWeka)
m.m5p <- M5P(quality ~ ., data = wine_train)

# 트리 출력
m.m5p

# 모델 성능 요약
summary(m.m5p)

# 모델에 대한 예측
p.m5p <- predict(m.m5p, wine_test)

# 예측에 대한 요약 통계
summary(p.m5p)

# 예측값과 실제값간의 상관 관계
cor(p.m5p, wine_test$quality)

# 예측값과 실제값의 평균 절대 오차
# (위에 정의한 함수 사용)
MAE(wine_test$quality, p.m5p)
