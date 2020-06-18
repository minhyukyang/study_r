# One-hot encoding in R: three simple methods

set.seed(555)
data <- data.frame(
  Outcome = seq(1,100,by=1),
  Variable = sample(c("Red","Green","Blue"), 100, replace = TRUE)
)

# Method 1: one_hot in mltools package
library(mltools)
library(data.table)

newdata <- one_hot(as.data.table(data))

head(newdata)
#    Outcome Variable_Blue Variable_Green Variable_Red
# 1:       1             0              1            0
# 2:       2             1              0            0
# 3:       3             1              0            0
# 4:       4             0              1            0
# 5:       5             0              1            0
# 6:       6             0              0            1

# Method 2: dummyVars in caret package
library(caret)

dummy <- dummyVars(" ~ .", data=data)
newdata <- data.frame(predict(dummy, newdata = data)) 

head(newdata)
#   Outcome Variable.Blue Variable.Green Variable.Red
# 1       1             0              1            0
# 2       2             1              0            0
# 3       3             1              0            0
# 4       4             0              1            0
# 5       5             0              1            0
# 6       6             0              0            1

# Method 3: dcast in reshape2 package
library(reshape2)

newdata <- dcast(data = data, Outcome ~ Variable, length)

head(newdata)
#  Outcome Blue Green Red
# 1       1    0     1   0
# 2       2    1     0   0
# 3       3    1     0   0
# 4       4    0     1   0
# 5       5    0     1   0
# 6       6    0     0   1