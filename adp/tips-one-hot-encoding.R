setwd("d:/Analysis/R/study_r/adp")

# 머신러닝 문제 해결 과정에서는 다양한 데이터 전처리(Preprocessing) 스킬이 요구되며, 변수 타입별로 각각 다른 방법을 적용해야 합니다.
# 이번 글에서는 범주형 변수(Categorical Variable) 처리 방법 중 One-Hot Encoding에 대한 내용을 소개합니다.

# ### 1. One-Hot Encoding?
# 

# 본 글에서는 범주형 데이터 처리 방법 중, 
# 이 포스팅은 Kaggle의 California Housing Prices 문제 풀이를 따라 연습한 글입니다.
# 해당 포스트에 문제가 있을 시 알려주시면 수정하겠습니다.
# 출처 : [kaggle - California Housing Prices](https://www.kaggle.com/camnugent/introduction-to-machine-learning-in-r-tutorial)


# Tips --------------------------------------------------------------------

data <- housing %>%
  mutate(mean_bedrooms = (total_bedrooms / households),
         mean_rooms = (total_rooms / households)) %>%
  select(-c(total_bedrooms, total_rooms)) %>%
  mutate(near_bay = ifelse(ocean_proximity == "NEAR BAY", 1, 0),
         h_ocean = ifelse(ocean_proximity == "<1H OCEAN", 1, 0),
         inland = ifelse(ocean_proximity == "INLAND", 1, 0),
         near_ocean = ifelse(ocean_proximity == "NEAR OCEAN", 1, 0),
         island = ifelse(ocean_proximity == "ISLAND", 1, 0))


# Tips2 -------------------------------------------------------------------

# Set up matrix of zeroes
cat_housing <- data.frame(matrix(0, nrow = nrow(housing), ncol = length(unique(housing$ocean_proximity))))

# rename columns using factor levels in ocean_proximity
colnames(cat_housing) <- as.character(unique(housing$ocean_proximity))

# use sapply and ifelse to set value equal to one when the value in ocean_proximity is equal to the column name
cat_housing[] <- sapply(seq_along(cat_housing), function(x) ifelse(names(cat_housing[x]) == as.character(housing$ocean_proximity),1,0))

# Tips3

#--------------------------------
# https://datatricks.co.uk/one-hot-encoding-in-r-three-simple-methods
set.seed(555)
data <- data.frame(
  Outcome = seq(1,100,by=1),
  Variable = sample(c("Red","Green","Blue"), 100, replace = TRUE)
)
# Method 1: one_hot in mltools package
library(mltools)
library(data.table)

newdata <- one_hot(as.data.table(data))
# Method 2: dummyVars in caret package
library(caret)

dummy <- dummyVars(" ~ .", data=data)
newdata <- data.frame(predict(dummy, newdata = data)) 
# Method 3: dcast in reshape2 package
library(reshape2)

newdata <- dcast(data = data, Outcome ~ Variable, length)
