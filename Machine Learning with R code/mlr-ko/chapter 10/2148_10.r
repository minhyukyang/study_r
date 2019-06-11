##### 10장 : 모델 성능 평가 -------------------

## R의 혼돈 매트릭스 ----
sms_results <- read.csv("sms_results.csv")
head(sms_results)
head(subset(sms_results, actual_type != predict_type))

# 벡터 명시
table(sms_results$actual_type, sms_results$predict_type)
# 공식 적용
xtabs(~ actual_type + predict_type, sms_results)
# CrossTable함수 사용
library(gmodels)
CrossTable(sms_results$actual_type, sms_results$predict_type)

# 정확도(accuracy)와 오차 비율(error rate) 계산 --
# 정확도
(154 + 1202) / (154 + 1202 + 5 + 29)
# 오차 비율
(5 + 29) / (154 + 1202 + 5 + 29)
# 오차 비율 = 1 - 정확도
1 - 0.9755396

## 정확도 이외 다른 성능 측정 ----
library(caret)
confusionMatrix(sms_results$predict_type, sms_results$actual_type, positive = "spam")

# 카파 통계 (Kappa statistic)
# SMS 분류기를 활용한 예제
pr_a <- 0.865 + 0.111
pr_a

pr_e <- 0.868 * 0.886 + 0.132 * 0.114
pr_e

k <- (pr_a - pr_e) / (1 - pr_e)
k

library(vcd)
Kappa(table(sms_results$actual_type, sms_results$predict_type))

library(irr)
kappa2(sms_results[1:2])

# 민감도(Sensitivity)와 특이도(specificity)
# SMS 분류기를 활용한 예제
sens <- 154 / (154 + 29)
sens

spec <- 1202 / (1202 + 5)
spec

# caret 패키지를 활용한 예제
library(caret)
sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")
specificity(sms_results$predict_type, sms_results$actual_type, negative = "ham")

# 정밀도(Precision)과 재현율(recall)
prec <- 154 / (154 + 5)
prec

rec <- 154 / (154 + 29)
rec

# caret 패키지를 활용한 예제
library(caret)
posPredValue(sms_results$predict_type, sms_results$actual_type, positive = "spam")
sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")

# F-측정(F-measure)
f <- (2 * prec * rec) / (prec + rec)
f

f2 <- (2 * 154) / (2 * 154 + 5 + 29)
f2

## 성능 균형의 시각화 ----
library(ROCR)
pred <- prediction(predictions = sms_results$prob_spam,
                   labels = sms_results$actual_type)

# ROC 커브
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve for SMS spam filter", col = "blue", lwd = 2)

# 그래프에 참고선 추가
abline(a = 0, b = 1, lwd = 2, lty = 2)

# AUC 계산
perf.auc <- performance(pred, measure = "auc")
str(perf.auc)
as.numeric(perf.auc@y.values)

# 데이터 분활
library(caret)
credit <- read.csv("credit.csv")

# Holdout 기법
# 무작위 ID 사용
random_ids <- order(runif(1000))
credit_train <- credit[random_ids[1:500],]
credit_validate <- credit[random_ids[501:750], ]
credit_test <- credit[random_ids[751:1000], ]

# caret 함수 사용
in_train <- createDataPartition(credit$default, p = 0.75, list = FALSE)
credit_train <- credit[in_train, ]
credit_test <- credit[-in_train, ]

# 10-중첩 CV
folds <- createFolds(credit$default, k = 10)
str(folds)
credit01_train <- credit[folds$Fold01, ]
credit01_test <- credit[-folds$Fold01, ]

## lapply()을 사용한 C5.0 결정 트리에 대한 10-중첩 CV 자동화 ----
library(caret)
library(C50)
library(irr)

set.seed(123)
folds <- createFolds(credit$default, k = 10)

cv_results <- lapply(folds, function(x) {
  credit_train <- credit[x, ]
  credit_test <- credit[-x, ]
  credit_model <- C5.0(default ~ ., data = credit_train)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
  return(kappa)
})

str(cv_results)
mean(unlist(cv_results))
