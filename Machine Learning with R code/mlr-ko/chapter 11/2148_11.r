##### 11장 : 모델 성능 향상 -------------------

# 신용 데이터셋 로드
credit <- read.csv("credit.csv")
library(caret)

## 단순한 조율된 모델 생성 ----
# C5.0 결정 트리의 매개 변수 자동 조절
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0")

# 조율된 결과 요약
m

# 예측하기 위해 최적의 C5.0 후보 모델 적용
p <- predict(m, credit)
table(p, credit$default)

# 예측 분류 결과
head(predict(m, credit, type = "raw"))

# 예측 확률 결과
head(predict(m, credit, type = "prob"))

## 조율 과정 변경 ----
# 리샘플링(resampling) 전략 조절을 위한 trainControl() 사용
ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "oneSE")

# 조율하는 매개변수의 격자 생성을 위한 expand.grid() 사용
grid <- expand.grid(.model = "tree",
                    .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    .winnow = "FALSE")

# expand.grid() 결과 확인
grid

# 컨트롤 리스트와 매개 변수 격자를 사용하여  train() 변경
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)
m

## Bagging ----
# ipred bagged 결정 트리(decision trees) 사용
library(ipred)
set.seed(300)
mybag <- bagging(default ~ ., data = credit, nbagg = 25)
credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)

# ipred bagged trees의 성능 추정
library(caret)
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag",
      trControl = ctrl)

# caret의 좀 더 일반적인 bagging 함수 사용
# svmBag을 사용한 bag control object 생성
str(svmBag)
svmBag$fit

bagctrl <- bagControl(fit = svmBag$fit,
                      predict = svmBag$pred,
                      aggregate = svmBag$aggregate)

# bagged svm model 모델 적합화
set.seed(300)
svmbag <- train(default ~ ., data = credit, "bag",
                trControl = ctrl, bagControl = bagctrl)

svmbag

## Boosting ----
# AdaBoost.M1 사용
# (이 예제는 책에서 다루지 않음)
library(adabag)
set.seed(300)
bst <- boosting.cv(default ~ ., data = credit, mfinal = 50)
bst$confusion
bst$error

## Random Forests ----
# 기본 설정의 random forest
library(randomForest)
set.seed(300)
rf <- randomForest(default ~ ., data = credit)
rf

library(caret)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10)

# random forest의 자동 조율
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))

set.seed(300)
m_rf <- train(default ~ ., data = credit, method = "rf",
              metric = "Kappa", trControl = ctrl,
              tuneGrid = grid_rf)
m_rf

# boosted C5.0 결정 트리의 자동 조율
grid_c50 <- expand.grid(.model = "tree",
                        .trials = c(10, 20, 30, 40),
                        .winnow = "FALSE")

set.seed(300)
m_c50 <- train(default ~ ., data = credit, method = "C5.0",
                metric = "Kappa", trControl = ctrl,
               tuneGrid = grid_c50)
m_c50