setwd("d:/Analysis/R/study_r/adp/california-housing-prices/src/")

# 데이터 분할하기(train / validation / test)

# 1. 데이터 불러오기 
library(tidyverse)
library(reshape2)

housing <- read.csv("../input/housing.csv")

# 2. 데이터 분할하기
set.seed(2019)

# 2-1. Index 생성

idx <- sample(x = c("train", "valid", "test"),
              size = nrow(housing),
              replace = TRUE,
              prob = c(0.6, 0.2, 0.2))


# 2-2. 데이터 분리

train <- housing[idx == "train", ]
valid <- housing[idx == "valid", ]
test <- housing[idx == "test", ]

# checking
nrow(housing)  == nrow(train)+nrow(valid)+nrow(test)
# TRUE

# Reference 
# [1] https://kkokkilkon.tistory.com/13
# [2] https://rfriend.tistory.com/188