# 5. 모형 평가

set.seed(2020)

data(iris)

# 1. 오분류표(Confusion Matrix) -----------------------------------------------

iris2 <- subset(iris, Species == "setosa" | Species == "versicolor")
iris2$Species <- factor(iris2$Species)
str(iris2)

ind <- sample(2, nrow(iris2), replace=TRUE, prob=c(0.7, 0.3))

trainData <- iris2[ind==1,]
testData <- iris2[ind==2,]

library(nnet)
library(rpart)

nn_iris <- nnet(Species~., data=trainData, size=2, rang=0.1, decay=5e-4, maxit=200) # Neural Net
dt_iris <- rpart(Species~., data=trainData) # Decision Tree

nn_pred <- predict(nn_iris, testData, type="class")
dt_pred <- predict(dt_iris, testData, type="class")

library(caret)

nn_con <- confusionMatrix(factor(nn_pred), testData$Species)
dt_con <- confusionMatrix(dt_pred, testData$Species)

nn_con$table
#             Reference
# Prediction   setosa versicolor
# setosa         14          0
# versicolor      0         15

dt_con$table
#             Reference
# Prediction   setosa versicolor
# setosa         14          0
# versicolor      0         15

accuracy <- c(nn_con$overall['Accuracy'], dt_con$overall['Accuracy']) # 정분류율
precision <- c(nn_con$byClass['Pos Pred Value'], dt_con$byClass['Pos Pred Value']) # 정확도
recall <- c(nn_con$byClass['Sensitivity'], dt_con$byClass['Sensitivity']) # 재현율
f1 <- 2 * ((precision * recall) / (precision + recall)) # F1-Score
result <- data.frame(rbind(accuracy, precision, recall, f1))
names(result) <- c("Nueral Network", "Decision Tree")
result
#           Nueral Network Decision Tree
# accuracy               1             1
# precision              1             1
# recall                 1             1
# f1                     1             1


# 2. ROC ------------------------------------------------------------------
# x축 : FP Ratio(1-특이도(specificity)), y축 : 민감도(sensitivity)
# 평가 : ROC 그래프의 밑부분 면적(Area Under the ROC Curve, AUC)이 넓을 수록 좋은 모형 (AUC가 1에 가까울수록 좋은 모형)

set.seed(2020)
infert <- infert[sample(nrow(infert)),]
infert <- infert[,c("age", "parity", "induced", "spontaneous", "case")]

trainData <- infert[1:(nrow(infert)*0.7),]
testData <- infert[((nrow(infert)*0.7)+1):nrow(infert),]

library(neuralnet)

net.infert <- neuralnet(case ~ age+parity+induced+spontaneous, data=trainData, hidden=3, err.fct="ce", linear.output=FALSE, likelihood=TRUE)
n_test <- subset(testData, select=-case)
nn_pred <- compute(net.infert, n_test)
testData$net_pred <- nn_pred$net.result
head(testData)
#     age parity induced spontaneous case  net_pred
# 174  21      1       1           0    0 0.3494399
# 140  28      2       1           1    0 0.5028235
# 17   30      5       2           1    1 0.5028235
# 30   30      4       2           0    1 0.5012223
# 95   37      4       1           1    0 0.5028235
# 160  25      1       0           0    0 0.1696050

# install.packages("C50")
library(C50) # decision tree

trainData$case <- factor(trainData$case)
dt.infert <- C5.0(case ~ age+parity+induced+spontaneous, data=trainData)
testData$dt_pred <- predict(dt.infert, testData, type="prob")[,2]
head(testData)
#     age parity induced spontaneous case  net_pred   dt_pred
# 174  21      1       1           0    0 0.3494399 0.2031379
# 140  28      2       1           1    0 0.5028235 0.8323699
# 17   30      5       2           1    1 0.5028235 0.3330369
# 30   30      4       2           0    1 0.5012223 0.2031379
# 95   37      4       1           1    0 0.5028235 0.3330369
# 160  25      1       0           0    0 0.1696050 0.2031379

# install.packages("Epi")
library(Epi)

neural_ROC <- ROC(form=case~net_pred, data=testData, plot="ROC")
neural_ROC$AUC # 0.6828571

dtree_ROC <- ROC(form=case~dt_pred, data=testData, plot="ROC")
dtree_ROC$AUC # 0.7334694
