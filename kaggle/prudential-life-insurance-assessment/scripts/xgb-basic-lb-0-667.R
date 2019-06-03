library(readr)
library(dplyr)
library(xgboost)
library(caret)

seed <- 2131

train = read_csv("../input/train.csv")
test = read_csv("../input/test.csv")

print(dim(train))
print(head(train, n=5))
print(dim(test))
print(head(test, n=5))

test$Response = 0

testId = test$Id
train$Id = test$Id = NULL

train[is.na(train)] <- -1
test[is.na(test)] <- -1

train$Product_Info_2_char = as.factor(substr(train$Product_Info_2, 1,1))
train$Product_Info_2_num = as.factor(substr(train$Product_Info_2, 2,2))
test$Product_Info_2_char = as.factor(substr(test$Product_Info_2, 1,1))
test$Product_Info_2_num = as.factor(substr(test$Product_Info_2, 2,2))

train$BMI_Age <- train$BMI * train$Ins_Age
test$BMI_Age <- test$BMI * test$Ins_Age

response <- train$Response
train$Response <- NULL

train$Medical_History_10 <- NULL
train$Medical_History_24 <- NULL

test$Medical_History_10 <- NULL
test$Medical_History_24 <- NULL

feature.names <- colnames(train)

for (f in feature.names) {
    if (class(train[[f]])=="character") {
        levels <- unique(c(train[[f]], test[[f]]))
        train[[f]] <- as.integer(factor(train[[f]], levels=levels))
        test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
    }
}

dtrain<-xgb.DMatrix(data=data.matrix(train[,feature.names]),label=response, missing=NA)
watchlist<-list(val=dtrain,train=dtrain)

param <- list(  objective           = "reg:linear", 
                booster             = "gbtree",
                eta                 = 0.05, # 0.06, #0.01,
                max_depth           =  6, #changed from default of 8
                subsample           = 0.8, # 0.7
                min_child_weight    = 25,
                colsample_bytree    = 0.7, # 0.7
                silent              = 0
)

set.seed(seed)
clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 500, 
                    verbose             = 1,  
                    print.every.n       = 10,
                    watchlist           = watchlist,
                    maximize            = FALSE
)

dtest<-xgb.DMatrix(data=data.matrix(test[,feature.names]), missing = NA)

submission = read_csv("../input/sample_submission.csv")
pred <- predict(clf, dtest)
submission$Response <- pred

responseOrder <- c(rep.int(1, 1505), 
                   rep.int(2, 992), 
                   rep.int(3, 1541), 
                   rep.int(4, 2117), 
                   rep.int(5, 2244),
                   rep.int(6, 2096),
                   rep.int(7, 3150),
                   rep.int(8, 6120))

submission <- submission[order(submission$Response, decreasing = FALSE),]
submission <- cbind(submission, responseOrder)
submission <- submission[order(submission$Id, decreasing = FALSE),]
submission$Response <- NULL

colnames(submission) <- c("Id", "Response")
table(submission$Response)
write_csv(submission, "2016_02_13_04.csv")










