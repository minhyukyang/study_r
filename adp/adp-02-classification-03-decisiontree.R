# 3. 의사결정나무 모형 

set.seed(2020)
data(iris)

library(caret)

idx <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
# ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))

trainData <- iris[idx,]
testData <- iris[-idx,]


# 1. tree -----------------------------------------------------------------

library(tree)

myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

# Model
iris_tree <- tree(myFormula, data=trainData)
plot(iris_tree)
text(iris_tree)

# Pruning
cv.trees <- cv.tree(iris_tree, FUN=prune.misclass)
plot(cv.trees) # 분산이 가장 낮은 클래스 선택

prune.trees <- prune.misclass(iris_tree, best=3)
plot(prune.trees)
text(prune.trees)

# Prediction & Evaluation
library(e1071)

treepred <- predict(prune.trees, testData, type='class')
confusionMatrix(treepred, testData$Species) # 0.9333


# 2. rpart ----------------------------------------------------------------

library(rpart)

myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

# Model
iris_rpart <- rpart(myFormula, data = trainData, method="class")
# iris_rpart <- rpart(myFormula, data = iris, control = rpart.control(minsplit = 10))
plot(iris_rpart)
text(iris_rpart)

printcp(iris_rpart) # xerror이 가장 낮은 split 개수 선택
#        CP nsplit rel error   xerror     xstd
# 1 0.50000      0  1.000000 1.142857 0.062348
# 2 0.45714      1  0.500000 0.742857 0.073189
# 3 0.01000      2  0.042857 0.085714 0.033978

plotcp(iris_rpart)

# Pruning
ptree <- prune(iris_rpart, cp=iris_rpart$cptable[which.min(iris_rpart$cptable[,"xerror"]),])
plot(ptree)
text(ptree, use.n=TRUE)

# Prediction & Evaluation
rpartpred <- predict(ptree, testData, type="class")
confusionMatrix(rpartpred, testData$Species) # 0.9333         

# `$cptable`은 트리의 크기에 따른 비용-복잡도 모수를 제공하며, 교차 타당성 오차를 함께 제공함
# 이 값들은 `prune()` 또는 `rpart.control()` 함수에서 가지치기와 트리의 최대 크기를 조절하기 위한 옵션으로 사용됨


# 3. party ----------------------------------------------------------------

library(party)

myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

# Model
iris_ctree <- ctree(myFormula, data=trainData)
plot(iris_ctree)
plot(iris_ctree, type="simple")

# Prediction & Evaluation
partypred <- predict(iris_ctree, testData)
confusionMatrix(partypred, testData$Species) # 0.8889

# party 패키지는 prune 작업 필요 없으나 입력변수 제한 있음