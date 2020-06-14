# 3. 의사결정나무 모형 

set.seed(2020)

data(iris)

ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))

trainData <- iris[ind==1,]
testData <- iris[ind==2,]

# 1. rpart ----------------------------------------------------------------

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


# 2. party ----------------------------------------------------------------

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



