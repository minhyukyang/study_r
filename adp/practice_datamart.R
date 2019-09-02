# 데이터 마트
# 참고: https://blog.naver.com/moontaehyeon/221376841488

# 1. reshape --------------------------------------------------------------
library(reshape)

data(airquality)
head(airquality)
#   ozone solar.r wind temp month day
# 1    41     190  7.4   67     5   1
# 2    36     118  8.0   72     5   2
# 3    12     149 12.6   74     5   3
# 4    18     313 11.5   62     5   4
# 5    NA      NA 14.3   56     5   5
# 6    28      NA 14.9   66     5   6
names(airquality) # 데이터의 변수명 보기
# [1] "Ozone"   "Solar.R" "Wind"    "Temp"    "Month"   "Day"

names(airquality) <- tolower(names(airquality)) # 변수명을 모두 소문자로 변경
aqm <- melt(airquality, id = c("month", "day"), na.rm = TRUE)
head(aqm)
#   month day variable value
# 1     5   1    ozone    41
# 2     5   2    ozone    36
# 3     5   3    ozone    12
# 4     5   4    ozone    18
# 5     5   6    ozone    28
# 6     5   7    ozone    23
# airquality 데이터가 월과 일 기준으로 재정렬됨

a <- cast(aqm, day ~ month ~ variable)
# , , variable = ozone
#
#   month
# day    5  6   7   8  9
# 1   41 NA 135  39 96
# 2   36 NA  49   9 78
# 3   12 NA  32  16 73
# 4   18 NA  NA  78 91
# 5   NA NA  64  35 47
# melt 함수로 정렬된 데이터를 month를 x축, day를 y축으로 재구성
# 이때, names에서 정의되어 있는 변수별로 따로 정렬

b <- cast(aqm, month ~ variable, mean)
#   month    ozone  solar.r      wind     temp
# 1     5 23.61538 181.2963 11.622581 65.54839
# 2     6 29.44444 190.1667 10.266667 79.10000
# 3     7 59.11538 216.4839  8.941935 83.90323
# 4     8 59.96154 171.8571  8.793548 83.96774
# 5     9 31.44828 167.4333 10.180000 76.90000
# 각 변수의 일별 데이터에 대하여 월별 평균을 계산

# margins 옵션 : 각 행과 열에 대한 소계를 산출할 수 있음 (아래 식에서 변수와 상관없이 단순히 행과 열 데이터에 대한 소계 산출)
c <-
  cast(aqm, month ~ variable, mean, margins = c("grand_row", "grand_col"))
#   month    ozone  solar.r      wind     temp    (all)
# 1     5 23.61538 181.2963 11.622581 65.54839 68.70696
# 2     6 29.44444 190.1667 10.266667 79.10000 87.38384
# 3     7 59.11538 216.4839  8.941935 83.90323 93.49748
# 4     8 59.96154 171.8571  8.793548 83.96774 79.71207
# 5     9 31.44828 167.4333 10.180000 76.90000 71.82689
# 6 (all) 42.12931 185.9315  9.957516 77.88235 80.05722

# subset 옵션 : 일부 데이터를 추출하여 처리하는 기능
d <- cast(aqm, day ~ month, subset = variable == "ozone")
# day   5  6   7   8  9
# 1    1  41 NA 135  39 96
# 2    2  36 NA  49   9 78
# 3    3  12 NA  32  16 73
# 4    4  18 NA  NA  78 91
# 5    5  NA NA  64  35 47

# range 옵션 : 지정된 factor에 대해 변수별 최소, 최대값 추출
e <- cast(aqm, month ~ variable, range)
#   month ozone_X1 ozone_X2 solar.r_X1 solar.r_X2 wind_X1 wind_X2 temp_X1 temp_X2
# 1     5        1      115          8        334     5.7    20.1      56      81
# 2     6       12       71         31        332     1.7    20.7      65      93
# 3     7        7      135          7        314     4.1    14.9      73      92
# 4     8        9      168         24        273     2.3    15.5      72      97
# 5     9        7       96         14        259     2.8    16.6      63      93


# 2. plyr -----------------------------------------------------------------
library(plyr)

d <-
  data.frame(year = rep(2012:2014, each = 6), count = round(runif(9, 0, 20)))
#    year count
# 1  2012    20
# 2  2012    16
# 3  2012     5
# 4  2012    20
# 5  2012    19
# 6  2012    15
# 7  2013    15
# 8  2013    20
# 2012년부터 2014년까지 각 년도를 6번 반복 출력 year 변수로 지정
# 0에서 20까지의 숫자 중 9개의 난수를 발생하여 count 변수로 지정

ddply(d, "year", function(x) {
  mean.count = mean(x$count)
  sd.count = sd(x$count)
  cv = sd.count / mean.count
  data.frame(cv.count = cv)
})
#   year  cv.count
# 1 2012 0.3604245
# 2 2013 0.3967765
# 3 2014 0.2168871
# 데이터프레임 d에서 year 변수 별로의 count 값을 입력받아서 수식을 계산
# year 변수 별로 계산된 값을 cv.count에 저장하고 year 변수와 data.frame을 구성하여 출력
# data.frame는 열단위로 받아들이고 출력함
# sd(d$count[d$year=="2012"])/mean(d$count[d$year=="2012"]) = 0.3604245

ddply(d, "year", summarise, mean.count = mean(count))
#    year mean.count
# 1 2012   15.83333
# 2 2013   14.50000
# 3 2014   16.66667
# year 변수 별로 count 값의 평균을 구하여 데이터프레임 형태로 출력

ddply(d, "year", transform, total.count = sum(count))
#     year count total.count
# 1  2012    20          95
# 2  2012    16          95
# 3  2012     5          95
# 4  2012    20          95
# 5  2012    19          95
# 6  2012    15          95
# 7  2013    15          87
# 8  2013    20          87
# 9  2013    11          87
# 10 2013    20          87
# 11 2013    16          87
# 12 2013     5          87
# 13 2014    20         100
# 14 2014    19         100
# 15 2014    15         100
# 16 2014    15         100
# 17 2014    20         100
# 18 2014    11         100
# year 변수 별로 count 값을 합하고, total.count 변수로 지정하여 별도을 추가하여 데이터프레임 형태로 출력


# 3. data.table -----------------------------------------------------------
library(data.table)

DT <- data.table(x = c("d", "b", "b", "a", "a"), v = rnorm(5))
DT
#   x          v
# 1: d -2.2112975
# 2: b -0.6689478
# 3: b -0.2801792
# 4: a  0.5710815
# 5: a -0.6549359
# data.table은 row number가 콜론(:)으로 출력되며, 동작은 data.frame과 동일


# 4. example : dplyr ------------------------------------------------------

# 데이터 만들기
set.seed(7)
group <- rep(c(1, 2), c(52,52))
disease <- c(rbinom(n = 104, size = 1, prob = 0.25)) + 1

dat <- data.frame(group, disease)
head(dat)
#    group disease
# 1     1       1
# 2     1       1
# 3     1       2
# 4     1       1
# 5     1       2
# 6     1       1

# 데이터프레임 행렬 바꾸기
# Matrix Transpose
tab <- table(dat)
tab_t <- t(tab)
#       group
# disease  1  2
#       1 35 38
#       2 17 14

# 조건식을 포함한 변수열 추출
# 1) indexing & which() 함수를 활용한 특정 조건을 만족하는 변수 선택
attach(mtcars)
mtcars_mart_0 <- mtcars[which(am == 0 & cyl == c(4, 6)), c("mpg", "cyl", "am")]
mtcars_mart_0
#                mpg cyl am
# Hornet 4 Drive 21.4   6  0
# Valiant        18.1   6  0
# Merc 230       22.8   4  0
# Merc 280       19.2   6  0
# Toyota Corona  21.5   4  0

mean(mtcars_mart_0$mpg)
# [1] 20.6

# 변수 선택시 변수명을 직접 입력해도 되고 열의 위치를 숫자로 c(1, 2, 9)라고 입력 가능
mtcars_mart_9 <- mtcars[which(am == 0 & cyl == c(4, 6)), c(1, 2, 9)]
#                mpg cyl am
# Hornet 4 Drive 21.4   6  0
# Valiant        18.1   6  0
# Merc 230       22.8   4  0
# Merc 280       19.2   6  0
# Toyota Corona  21.5   4  0
detach(mtcars)

# 2) subset(dataset이름, select=c(변수명), sebset=(선별조건))을 이용한 변수 선택
mtcars_subset_0 <-  subset(mtcars,
                           select = c(mpg, cyl, am),
                           subset = (am == 0 & cyl == c(4, 6)))
#               mpg cyl am
# Mazda RX4 Wag 21.0   6  1
# Datsun 710    22.8   4  1
# Honda Civic   30.4   4  1
# Porsche 914-2 26.0   4  1
# Ferrari Dino  19.7   6  1

mean(mtcars_subset_0$mpg)
# [1] 20.6
