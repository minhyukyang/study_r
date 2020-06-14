# 4. 앙상블 모형

set.seed(2020)

data(iris)

ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))

trainData <- iris[ind==1,]
testData <- iris[ind==2,]

# 1. bagging --------------------------------------------------------------
# install.packages("adabag")
library(adabag)

iris.bagging <- bagging(Species ~ ., data=trainData, mfinal=10)

iris.bagging$importance
# Petal.Length  Petal.Width Sepal.Length  Sepal.Width 
# 91.079362     8.920638     0.000000     0.000000 

plot(iris.bagging$trees[[10]])
text(iris.bagging$trees[[10]])

pred <- predict(iris.bagging, newdata=testData)
table(pred$class, testData[,5])
#             setosa versicolor virginica
# setosa         17          0         0
# versicolor      0          9         1
# virginica       0          1        14


# 2. boosting -------------------------------------------------------------
library(adabag)

data(iris)

iris.adabag <- boosting(Species ~., data=trainData, boos=TRUE, mfinal=10)
iris.adabag$importance
# Petal.Length  Petal.Width Sepal.Length  Sepal.Width 
# 66.501136    18.444748     6.893400     8.160716 

plot(iris.adabag$trees[[10]])
text(iris.adabag$trees[[10]])

pred <- predict(iris.adabag, newdata=testData)
(tb <- table(pred$class, testData[,5]))
#             setosa versicolor virginica
# setosa         17          0         0
# versicolor      0         10         0
# virginica       0          0        15


# 3. Random Forest --------------------------------------------------------
library(randomForest)

rf <- randomForest(Species ~ ., data=trainData, ntree=100, proximity=TRUE)

table(predict(rf), trainData$Species)
#            setosa versicolor virginica
# setosa         33          0         0
# versicolor      0         37         5
# virginica       0          3        30

print(rf)
# Call:
# randomForest(formula = Species ~ ., data = trainData, ntree = 100,      proximity = TRUE) 
# Type of random forest: classification
# Number of trees: 100
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 7.41%
# Confusion matrix:
#   setosa versicolor virginica class.error
# setosa         33          0         0   0.0000000
# versicolor      0         37         3   0.0750000
# virginica       0          5        30   0.1428571

plot(rf)

importance(rf) # Gini값이 클수록 중요한 변수
#              MeanDecreaseGini
# Sepal.Length         6.982548
# Sepal.Width          1.743684
# Petal.Length        37.392857
# Petal.Width         24.819429

varImpPlot(rf) 
# 해당 변수로부터 분할이 일어날 때 불순도(impurity)의 감소가 얼마나 일어나는지 나타내는 값
# 불순도 감소가 클수록 순수도 증가 => 지니 지수(Gini index)는 노드의 불순도

rf.pred <- predict(rf, newdata=testData)
table(rf.pred, testData$Species)
# irisPred     setosa versicolor virginica
# setosa         17          0         0
# versicolor      0         10         0
# virginica       0          0        15

# 4. caret
# https://thebook.io/006723/ch10/08/
library(caret)

(m <- train(Species ~., data=trainData,
            preProcess=c("pca"), method="rf", ntree=1000,
            trControl=trainControl(method="cv", number=10, repeats=3)))
# Random Forest 
# 
# 108 samples
# 4 predictor
# 3 classes: 'setosa', 'versicolor', 'virginica' 
# 
# Pre-processing: principal component signal extraction (4), centered (4), scaled (4) 
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 97, 98, 97, 97, 96, 97, ... 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy   Kappa    
# 2     0.9263636  0.8888246
# 3     0.9354545  0.9025746
# 4     0.9354545  0.9025746
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 3.

confusionMatrix(
  predict(m, newdata=testData, type="raw"),
  testData$Species)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   setosa versicolor virginica
# setosa         17          0         0
# versicolor      0          9         2
# virginica       0          1        13
# 
# Overall Statistics
# 
# Accuracy : 0.9286         
# 95% CI : (0.8052, 0.985)
# No Information Rate : 0.4048         
# P-Value [Acc > NIR] : 1.222e-12      
# 
# Kappa : 0.8909         
# 
# Mcnemar's Test P-Value : NA             
# 
# Statistics by Class:
# 
#                      Class: setosa Class: versicolor Class: virginica
# Sensitivity                 1.0000            0.9000           0.8667
# Specificity                 1.0000            0.9375           0.9630
# Pos Pred Value              1.0000            0.8182           0.9286
# Neg Pred Value              1.0000            0.9677           0.9286
# Prevalence                  0.4048            0.2381           0.3571
# Detection Rate              0.4048            0.2143           0.3095
# Detection Prevalence        0.4048            0.2619           0.3333
# Balanced Accuracy           1.0000            0.9187           0.9148