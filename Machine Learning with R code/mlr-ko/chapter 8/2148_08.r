##### 8장 : 연관 규칙(Association rules) -------------------

## 예제 : 자주 구매된 식료품 식별 ----
## 2단계 : 데이터 준비와 살펴보기 ----

# 식료품 데이터를 희소 매트릭스로 로드
library(arules)
groceries <- read.transactions("groceries.csv", sep = ",")
summary(groceries)

# 처음 5개 거래 확인
inspect(groceries[1:5])

# 식료품의 빈도 확인
itemFrequency(groceries[, 1:3])

# 식료품의 빈도 시각화
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)

# 처음 5개 거래에 대한 희소 매트릭스 시각화
image(groceries[1:5])

# 100개 식료품의 무작위 샘플 시각화
image(sample(groceries, 100))

## 3단계 : 데이터에 대한 모델 훈련 ----
library(arules)

# 기본 설정
apriori(groceries)

# 규칙을 좀 더 학습히기 위해 지지도(support)와 신뢰도(confidence) 설정 변경
groceryrules <- apriori(groceries, parameter = list(support =
                          0.006, confidence = 0.25, minlen = 2))
groceryrules

## 4단계 : 모델 성능 평가 ----
# 식료품 연관 규칙의 요약
summary(groceryrules)

# 처음 3개 규칙 확인
inspect(groceryrules[1:3])

## 5단계 : 모델 성능 향상 ----

# lift로 규칙 정렬
inspect(sort(groceryrules, by = "lift")[1:5])

# 딸기류 아이템을 포함하는 규칙의 부분 규칙 찾기
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

# CSV 파일에 규칙 쓰기
write(groceryrules, file = "groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

# 규칙들을 데이터 프레임으로 변환
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)
