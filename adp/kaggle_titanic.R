# 타이타닉 생존자 예측(kaggle)
# 참고 : https://www.kaggle.com/c/titanic
# R로 시작하는 kaggle : (1)https://blog.naver.com/bestinall/221507507813 (2)https://blog.naver.com/bestinall/221508794429
# [adp 실기] Titanic 데이터 실습 : (1)https://vvwwvw.tistory.com/30

# 0. set env --------------------------------------------------------------
library(tidyverse)
library(data.table)
setwd("u:/Analysis/R/study_r/adp/titanic/")

## Data Import
# titanic.train <- read.csv("train.csv", stringsAsFactors = FALSE, header = TRUE)
# titanic.test <- read.csv("test.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.train <- read_csv("train.csv")
titanic.test <- read_csv("test.csv")

## Data Structure
str(titanic.train)
str(titanic.test)

## Data Summary
summary(titanic.train) # 훈련용 데이터셋
# survived의 mean값이 0.3838인 것은 생존한 승객이 전체 약 38.4%라고 볼 수 있다.


# 1. EDA ------------------------------------------------------------------

## Data Visualization (Fare / Age)
titanic_fare <-
  ggplot(titanic.train,
         aes(x = Fare, y = ..density..),
         main = "",
         xlab = "Fare Distribution") + geom_density(fill = "blue", alpha = 0.2)
titanic_age <-
  ggplot(titanic.train,
         aes(x = Age, y = ..density..),
         main = "",
         xlab = "Age Distributuon") + geom_density(fill = "blue", alpha = 0.2)
grid.arrange(titanic_fare, titanic_age, nrow = 1, ncol = 2)
# 상대적으로 저렴한 요금(50달러 미만)의 승객 분포가 많았으며, 주로 20~40대 승객과
# 그들의 자녀로 추정되는 어린 연령의 승객이 많이 탑승한 것을 알 수 있다.

## mosaic plot
par(mfrow=c(1,2))
mosaicplot(table(ifelse(titanic.train$Survived==1, "Survived","Dead"), titanic.train$Sex), main="", cex=1.2, color = TRUE)
mosaicplot(table(ifelse(titanic.train$Survived==1, "Survived","Dead"), titanic.train$Pclass), main="", cex=1.2, color = TRUE)
# 생존한 승객 중 여성 승객의 비중이 높고, 상대적으로 1등급 티켓의 승객 생존 비중이 높게 나타났다.

## box plot / jitter plot
par(mfrow=c(1,2))
boxplot(Age ~ Survived, titanic.train, xlab="Survival", ylab="Age", cex=1.2)
plot(Age ~ jitter(Survived), titanic.train, cex=1.2, xlab="Survival")
# 박스플랏을 보면 생존자의 연령대가 전체적으로 조금 더 낮아 보이나, 중간값을 비교하였을때 그리 큰 차이가 없다.
# jitter 플랏의 결과 고연령의 승객은 생존률이 떨어지며, 어린이는 상대적으로 높다.

## scatter plot
ggplot(titanic.train, aes(Age, log(Fare), color = factor(Survived), shape = factor(Sex))) +
  geom_point() +  geom_jitter()
# 스캐터플롯으로 살펴보면, 어린이의 경우 티켓 등급이 낮아도 생존이 많은 반면,
# 연령이 높아지면서 비싼 요금의 승객의 생존이 높게 나타난다.


# 2. Preprocessing --------------------------------------------------------

# test데이터에 Survived 변수가 없기때문에 NA값으로 넣어준다.
# 이후 결측치 처리 이상치 처리 등등 데이터 변형을 위해 train 데이터와 test를 합쳐 fullDT로 만든다.

titanic.test$Survived <- NA

## fulLDT 데이터 형식 변경
fullDT <- rbindlist(list(titanic.train, titanic.test), use.names=TRUE)
str(fullDT)

## 명목형 변수 
fullDT$Pclass <- as.factor(fullDT$Pclass)
fullDT$Sex <- as.factor(fullDT$Sex)
fullDT$Cabin <- as.factor(fullDT$Cabin)
fullDT$Embarked <- as.factor(fullDT$Embarked)
fullDT$Survived <- as.factor(fullDT$Survived)

# names(Filter(is.factor, fullDT)) # 범주형 변수들 컬럼 확인

head(fullDT)
dim(fullDT)
# [1] 1309   12

## 결측치 확인 및 채우기
library(VIM)
VIM::aggr(fullDT, prop=FALSE, numbers=TRUE, sortVars= TRUE, cex.axis = .7)
# 데이터를 보면 Carbin(객실), Age(나이), Embarked(승선지역), Fare(요금)에 결측치가 있다.

## Fare(요금)
fullDT[is.na(Fare)]
#   PassengerId Survived Pclass               Name  Sex  Age SibSp Parch Ticket Fare Cabin Embarked
#1:        1044     <NA>      3 Storey, Mr. Thomas male 60.5     0     0   3701   NA  <NA>        S
mean(fullDT$Fare, na.rm = TRUE)
# [1] 33.29548
fullDT[is.na(Fare)]$Fare <- mean(fullDT$Fare, na.rm = TRUE) # NA에 mean값 넣기

# Fare에 결측치 개수는 1개로 결측치행을 탐색해 볼때 뚜렸한 인사이트를 얻기 힘들거 같다.
# 결측치 개수가 적으므로 승객들의 평균 Fare로 결측치를 채운다.

## Embarked(승선지역)
fullDT[is.na(Embarked)]
#    PassengerId Survived Pclass                                      Name    Sex Age SibSp Parch Ticket Fare Cabin Embarked
# 1:          62        1      1                       Icard, Miss. Amelie female  38     0     0 113572   80   B28     <NA>
# 2:         830        1      1 Stone, Mrs. George Nelson (Martha Evelyn) female  62     0     0 113572   80   B28     <NA>
fullDT[Ticket %in% 113572]
fullDT[Cabin %in% "B28"]
table(fullDT$Embarked)
#   C   Q   S 
# 270 123 914 
fullDT[is.na(Embarked)]$Embarked <- "S"
# Embarked의 결측치는 2개로 직접 탐색해 봤을 때, 2명 승객에 티켓과 객실을 사용함을 알 수 있다.
# 그래서 같은 티켓과 같은 객을 사용하는 다른 사람이 있는지 찾아 보았지만, 두사람 이외에는 없음을 알 수 있다.
# 결측치값을 대체하기 위한 방법은 여러가지 존재한다. 대표적으로는 평균값, 최빈값 등등 여러가지 존재하지만 데이터 특성에 따라 대체하는 방법은 분석가가 판단하는 문제이다. 
# 이 데이터에 인사트를 돌출하기 위해서는 많은 노력이 필요하기 때문에 범주형 데이터에서 흔히 사용한는 최빈값으로 대체할 생각이다.
# 이 결측치에서는 Embarked의 level이 3가지 범주 [1] "C" "Q" "S" 에서 빈도가 "S"가 914로 가장 크기 때문에 결측치는 "S"로 대체한다.

colSums(is.na(fullDT))
# PassengerId    Survived      Pclass        Name         Sex         Age       SibSp       Parch      Ticket        Fare       Cabin    Embarked 
#           0         418           0           0           0         263           0           0           0           0        1014           0

## Age(나이)
fullDT[is.na(Age)]
# PassengerId Survived Pclass                            Name    Sex Age SibSp Parch    Ticket    Fare Cabin Embarked
# 1:           6        0      3                Moran, Mr. James   male  NA     0     0    330877  8.4583  <NA>        Q
# 2:          18        1      2    Williams, Mr. Charles Eugene   male  NA     0     0    244373 13.0000  <NA>        S
# 3:          20        1      3         Masselmani, Mrs. Fatima female  NA     0     0      2649  7.2250  <NA>        C
# 4:          27        0      3         Emir, Mr. Farred Chehab   male  NA     0     0      2631  7.2250  <NA>        C
# 5:          29        1      3   O'Dwyer, Miss. Ellen "Nellie" female  NA     0     0    330959  7.8792  <NA>        Q
#  ---                                                                                                                    
# 259:        1300     <NA>      3 Riordan, Miss. Johanna Hannah"" female  NA     0     0    334915  7.7208  <NA>        Q
# 260:        1302     <NA>      3          Naughton, Miss. Hannah female  NA     0     0    365237  7.7500  <NA>        Q
# 261:        1305     <NA>      3              Spector, Mr. Woolf   male  NA     0     0 A.5. 3236  8.0500  <NA>        S
# 262:        1308     <NA>      3             Ware, Mr. Frederick   male  NA     0     0    359309  8.0500  <NA>        S
# 263:        1309     <NA>      3        Peter, Master. Michael J   male  NA     1     1      2668 22.3583  <NA>        C
fullDT[is.na(Age)]$Age <- median(fullDT$Age, na.rm=TRUE) # 나이 결측치는 중앙값(median)으로 채움

colSums(is.na(fullDT))
# PassengerId    Survived      Pclass        Name         Sex         Age       SibSp       Parch      Ticket        Fare       Cabin    Embarked 
#           0         332           0           0           0           0           0           0           0           0         774           0 

table(fullDT$Survived)

# 3. Model ----------------------------------------------------------------


