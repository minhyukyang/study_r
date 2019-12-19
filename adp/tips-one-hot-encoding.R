setwd("d:/Analysis/R/study_r/adp")

# 머신러닝 문제 해결 과정에서는 다양한 데이터 전처리(Preprocessing) 스킬이 요구되며, 변수 타입별로 각각 다른 방법을 적용해야 합니다.
# 이번 글에서는 범주형 변수(Categorical Variable) 처리 방법 중 **One-Hot Encoding**에 다양한 해결 방법들을 소개합니다.

# # 0. One-Hot Encoding?
# 데이터에는 수치형 데이터와 텍스트 데이터나 범주형 데이터가 있습니다. 머신러닝이나 딥러닝 알고리즘은 수치로 된 데이터만 이해할 수 있습니다.
# 그래서 기계가 이해할 수 있는 형태로 데이터를 변환해 주어야 하는데 범주형 데이터는 많은 경우 One Hot Encoding을 활용하여 변환해 줍니다.
# 원핫인코딩이란 해당되는 하나의 데이터만 1로 변경해 주고 나머지는 0으로 채워주는 것을 뜻합니다.
# - 출처 : [범주형 데이터 다루기 - 원핫인코딩](https://programmers.co.kr/learn/courses/21/lessons/11043)

# 데이터는 [kaggle - California Housing Prices](https://www.kaggle.com/camnugent/introduction-to-machine-learning-in-r-tutorial) 의 데이터를 사용하겠습니다.

housing <- read.csv("california-housing-prices/input/housing.csv")
head(housing)

# ### Case 1
library(dplyr)

data <- housing %>%
  mutate(mean_bedrooms = (total_bedrooms / households),
         mean_rooms = (total_rooms / households)) %>%
  select(-c(total_bedrooms, total_rooms)) %>%
  mutate(near_bay = ifelse(ocean_proximity == "NEAR BAY", 1, 0),
         h_ocean = ifelse(ocean_proximity == "<1H OCEAN", 1, 0),
         inland = ifelse(ocean_proximity == "INLAND", 1, 0),
         near_ocean = ifelse(ocean_proximity == "NEAR OCEAN", 1, 0),
         island = ifelse(ocean_proximity == "ISLAND", 1, 0))

head(housing)
#   longitude latitude housing_median_age total_rooms total_bedrooms population households median_income
# 1   -122.23    37.88                 41         880            129        322        126        8.3252
# 2   -122.22    37.86                 21        7099           1106       2401       1138        8.3014
# 3   -122.24    37.85                 52        1467            190        496        177        7.2574
# 4   -122.25    37.85                 52        1274            235        558        219        5.6431
# 5   -122.25    37.85                 52        1627            280        565        259        3.8462
# 6   -122.25    37.85                 52         919            213        413        193        4.0368
# median_house_value ocean_proximity
# 1             452600        NEAR BAY
# 2             358500        NEAR BAY
# 3             352100        NEAR BAY
# 4             341300        NEAR BAY
# 5             342200        NEAR BAY
# 6             269700        NEAR BAY

head(data)
# longitude latitude housing_median_age population households median_income median_house_value
# 1   -122.23    37.88                 41        322        126        8.3252             452600
# 2   -122.22    37.86                 21       2401       1138        8.3014             358500
# 3   -122.24    37.85                 52        496        177        7.2574             352100
# 4   -122.25    37.85                 52        558        219        5.6431             341300
# 5   -122.25    37.85                 52        565        259        3.8462             342200
# 6   -122.25    37.85                 52        413        193        4.0368             269700
# ocean_proximity mean_bedrooms mean_rooms near_bay h_ocean inland near_ocean island
# 1        NEAR BAY     1.0238095   6.984127        1       0      0          0      0
# 2        NEAR BAY     0.9718805   6.238137        1       0      0          0      0
# 3        NEAR BAY     1.0734463   8.288136        1       0      0          0      0
# 4        NEAR BAY     1.0730594   5.817352        1       0      0          0      0
# 5        NEAR BAY     1.0810811   6.281853        1       0      0          0      0
# 6        NEAR BAY     1.1036269   4.761658        1       0      0          0      0

# 출처 : [kaggle - California Housing Prices](https://www.kaggle.com/camnugent/introduction-to-machine-learning-in-r-tutorial)

# Tips2 -------------------------------------------------------------------

# Set up matrix of zeroes
cat_housing <- data.frame(matrix(0, nrow = nrow(housing), ncol = length(unique(housing$ocean_proximity))))

# rename columns using factor levels in ocean_proximity
colnames(cat_housing) <- as.character(unique(housing$ocean_proximity))

# use sapply and ifelse to set value equal to one when the value in ocean_proximity is equal to the column name
cat_housing[] <- sapply(seq_along(cat_housing), function(x) ifelse(names(cat_housing[x]) == as.character(housing$ocean_proximity),1,0))

head(cat_housing)
#   NEAR BAY <1H OCEAN INLAND NEAR OCEAN ISLAND
# 1        1         0      0          0      0
# 2        1         0      0          0      0
# 3        1         0      0          0      0
# 4        1         0      0          0      0
# 5        1         0      0          0      0
# 6        1         0      0          0      0


# Tips3

#--------------------------------
# https://datatricks.co.uk/one-hot-encoding-in-r-three-simple-methods
set.seed(555)
data <- data.frame(
  Outcome = seq(1,100,by=1),
  Variable = sample(c("Red","Green","Blue"), 100, replace = TRUE)
)

head(data)
# #  Outcome Variable
# 1       1    Green
# 2       2      Red
# 3       3      Red
# 4       4      Red
# 5       5      Red
# 6       6      Red

# Method 1: one_hot in mltools package
library(mltools)
library(data.table)

newdata <- one_hot(as.data.table(data))

head(newdata)
#    Outcome Variable_Blue Variable_Green Variable_Red
# 1:       1             0              1            0
# 2:       2             0              0            1
# 3:       3             0              0            1
# 4:       4             0              0            1
# 5:       5             0              0            1
# 6:       6             0              0            1

# Method 2: dummyVars in caret package
library(caret)

dummy <- dummyVars(" ~ .", data=data)
newdata <- data.frame(predict(dummy, newdata = data)) 

head(newdata)
 #  Outcome Variable.Blue Variable.Green Variable.Red
# 1       1             0              1            0
# 2       2             0              0            1
# 3       3             0              0            1
# 4       4             0              0            1
# 5       5             0              0            1
# 6       6             0              0            1

# Method 3: dcast in reshape2 package
library(reshape2)

newdata <- dcast(data = data, Outcome ~ Variable, length)

head(newdata)
#  Outcome Blue Green Red
# 1       1    0     1   0
# 2       2    0     0   1
# 3       3    0     0   1
# 4       4    0     0   1
# 5       5    0     0   1
# 6       6    0     0   1