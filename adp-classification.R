# decision tree
set.seed(2020)

# 1. party
# install.packages("party")
library(party)

data(iris)
summary(iris)
#  Sepal.Length    Sepal.Width     Petal.Length    Petal.Width          Species  
# Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100   setosa    :50  
# 1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300   versicolor:50  
# Median :5.800   Median :3.000   Median :4.350   Median :1.300   virginica :50  
# Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199                  
# 3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800                  
# Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500 

ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))

trainData <- iris[ind==1,]
testData <- iris[ind==2,]

myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

iris_ctree <- ctree(myFormula, data=trainData)

table(predict(iris_ctree), trainData$Species)
#            setosa versicolor virginica
# setosa         33          0         0
# versicolor      0         36         1
# virginica       0          4        34

print(iris_ctree)
#	 Conditional inference tree with 4 terminal nodes
# 
# Response:  Species 
# Inputs:  Sepal.Length, Sepal.Width, Petal.Length, Petal.Width 
# Number of observations:  108 
# 
# 1) Petal.Length <= 1.9; criterion = 1, statistic = 100.5
#   2)*  weights = 33 
# 1) Petal.Length > 1.9
#   3) Petal.Width <= 1.7; criterion = 1, statistic = 48.825
#     4) Petal.Length <= 4.8; criterion = 0.999, statistic = 12.872
#       5)*  weights = 37 
#     4) Petal.Length > 4.8
#       6)*  weights = 7 
#   3) Petal.Width > 1.7
#     7)*  weights = 31 

plot(iris_ctree)

plot(iris_ctree, type="simple")

testPred <- predict(iris_ctree, newdata=testData)

table(testPred, testData$Species)
# testPred     setosa versicolor virginica
# setosa         17          0         0
# versicolor      0          9         0
# virginica       0          1        15

# 2. rpart
library(rpart)

myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

bodyfat_rpart <- rpart(myFormula, data = iris, control = rpart.control(minsplit = 10))

attributes(bodyfat_rpart)
# $names
# [1] "frame"               "where"               "call"                "terms"               "cptable"             "method"             
# [7] "parms"               "control"             "functions"           "numresp"             "splits"              "variable.importance"
# [13] "y"                   "ordered"            
# 
# $xlevels
# named list()
# 
# $ylevels
# [1] "setosa"     "versicolor" "virginica" 
# 
# $class
# [1] "rpart"

print(bodyfat_rpart)
# n= 150 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 150 100 setosa (0.33333333 0.33333333 0.33333333)  
#   2) Petal.Length< 2.45 50   0 setosa (1.00000000 0.00000000 0.00000000) *
#   3) Petal.Length>=2.45 100  50 versicolor (0.00000000 0.50000000 0.50000000)  
#     6) Petal.Width< 1.75 54   5 versicolor (0.00000000 0.90740741 0.09259259)  
#       12) Petal.Length< 4.95 48   1 versicolor (0.00000000 0.97916667 0.02083333) *
#       13) Petal.Length>=4.95 6   2 virginica (0.00000000 0.33333333 0.66666667) *
#     7) Petal.Width>=1.75 46   1 virginica (0.00000000 0.02173913 0.97826087) *

plot(bodyfat_rpart)
text(bodyfat_rpart, use.n=TRUE)

bodyfat_rpart$cptable
#    CP nsplit rel error xerror       xstd
# 1 0.50      0      1.00   1.14 0.05230679
# 2 0.44      1      0.50   0.59 0.05982753
# 3 0.02      2      0.06   0.11 0.03192700
# 4 0.01      3      0.04   0.11 0.03192700

opt <- which.min(bodyfat_rpart$cptable[, "xerror"])
cp <- bodyfat_rpart$cptable[opt, "CP"]
prune.c <- prune(bodyfat_rpart, cp=cp)
# n= 150 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 150 100 setosa (0.33333333 0.33333333 0.33333333)  
# 2) Petal.Length< 2.45 50   0 setosa (1.00000000 0.00000000 0.00000000) *
#   3) Petal.Length>=2.45 100  50 versicolor (0.00000000 0.50000000 0.50000000)  
# 6) Petal.Width< 1.75 54   5 versicolor (0.00000000 0.90740741 0.09259259) *
#   7) Petal.Width>=1.75 46   1 virginica (0.00000000 0.02173913 0.97826087) *

plot(prune.c)
text(prune.c, use.n=T)
# `$cptable`은 트리의 크기에 따른 비용-복잡도 모수를 제공하며, 교차 타당성 오차를 함께 제공함
# 이 값들은 `prune()` 또는 `rpart.control()` 함수에서 가지치기와 트리의 최대 크기를 조절하기 위한 옵션으로 사용됨

plotcp(bodyfat_rpart) # cp값 출력

# 3. Random Forest
library(randomForest)

myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

rf <- randomForest(Species ~ ., data=trainData, ntree=100, proximity=TRUE)

table(predict(rf), trainData$Species)
#              setosa versicolor virginica
# setosa         33          0         0
# versicolor      0         37         4
# virginica       0          3        31

print(rf)
# Call:
# randomForest(formula = Species ~ ., data = trainData, ntree = 100,      proximity = TRUE) 
# Type of random forest: classification
# Number of trees: 100
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 6.48%
# Confusion matrix:
#            setosa versicolor virginica class.error
# setosa         33          0         0   0.0000000
# versicolor      0         37         3   0.0750000
# virginica       0          4        31   0.1142857

plot(rf)

importance(rf) # Gini값이 클수록 중요한 변수
#              MeanDecreaseGini
# Sepal.Length         6.182244
# Sepal.Width          1.815570
# Petal.Length        32.454116
# Petal.Width         30.522034

varImpPlot(rf)

irisPred <- predict(rf, newdata=testData)
table(irisPred, testData$Species)
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