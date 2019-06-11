##### 7장 : 신경망(Neural Network)과 서포트 벡터 머신(Support Vector Machines) -------------------

##### Part 1: 신경망 -------------------
## 예제 : 콘크리트의 강도 모델링  ----

## 2단계 : 데이터 준비와 살펴보기 ----
# 데이터 읽기와 구조 확인
concrete <- read.csv("concrete.csv")
str(concrete)

# 정규화 함수
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# 전체 데이터 프레임에 정규화 적용
concrete_norm <- as.data.frame(lapply(concrete, normalize))

# 0과1 사이에 범위 확인
summary(concrete_norm$strength)

# 본래 데이터의 최소값, 최대값 비교
summary(concrete$strength)

# 훈련과 테스트 데이터 생성
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

## 3단계 : 데이터로 모델 훈련 ----
# neuralnet 모델 훈련
library(neuralnet)

# 하나의 은닉 뉴런에 대한 단순한 ANN
concrete_model <- neuralnet(formula = strength ~ cement + slag +
                              ash + water + superplastic + 
                              coarseagg + fineagg + age,
                              data = concrete_train)


# 망(network) 시각화
plot(concrete_model)

## 4단계 : 모델 성능 평가 ----
# 모델 결과
model_results <- compute(concrete_model, concrete_test[1:8])
# 강도값 예측
predicted_strength <- model_results$net.result
# 예측값과 실제값간의 상관 관계 확인
cor(predicted_strength, concrete_test$strength)

## 5단계 : 모델 성능 향상 ----
# 5개 은닉 뉴런인 복잡한 뉴런망
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                               data = concrete_train, hidden = 5)

# 망(network) 시각화
plot(concrete_model2)

# 결과 평가
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

##### Part 2: 서포트 벡터 머신 -------------------
## 예제 : 광학식 문자 인식 ----

## 2단계 : 데이터 쥰비와 살펴보기 ----
# 데이터 읽기와 구조
letters <- read.csv("letterdata.csv")
str(letters)


# 훈련 데이터와 테스터 데이터 구분
letters_train <- letters[1:16000, ]
letters_test  <- letters[16001:20000, ]

## 3단계 : 데이터로 모델 훈련 ----
# 단순 선형 SVM을 훈련으로 시작
library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train,
                          kernel = "vanilladot")

# 모델에 대한 기본 정보 확인
letter_classifier

## 4단계 : 모델 성능 평가 ----
# 테스트 데이터셋에 대한 예측
letter_predictions <- predict(letter_classifier, letters_test)

head(letter_predictions)

table(letter_predictions, letters_test$letter)

# 일치/불일치 예측을 표시하는 TRUE/FALSE 벡터 생성
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))

## 5단계 : 모델 성능 향상 ----
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)

agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))
