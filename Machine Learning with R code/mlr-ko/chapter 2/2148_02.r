##### 2장 : 데이터 관리와 이해 -------------------

##### R 데이터 구조 --------------------

## 벡터(Vectors) -----

# 3명의 환자의 의료 데이터 벡터 생성
subject_name <- c("John Doe", "Jane Doe", "Steve Graves")
temperature <- c(98.1, 98.6, 101.4)
flu_status <- c(FALSE, FALSE, TRUE)

# 체온 벡터의 두 번째 원소
temperature[2]

## 벡터의 아이템에 접근하는 예제
# 2에서 3까지 원소 접근
temperature[2:3]

# 마이너스 기호를 사용한 2번째 원소 제외한 접근
temperature[-2]

# 아이템을 포함할지 지시하는 벡터를 사용
temperature[c(TRUE, TRUE, FALSE)]

## 팩터(Factors) -----

# 성별 팩터 추가
gender <- factor(c("MALE", "FEMALE", "MALE"))
gender

# 혈액형 팩터 추가
blood <- factor(c("O", "AB", "A"),
                levels = c("A", "B", "AB", "O"))
blood

## 리스트(Lists) -----

# 환자 정보 출력
subject_name[1]
temperature[1]
flu_status[1]
gender[1]
blood[1]

# 환자에 대한 리스트 생성
subject1 <- list(fullname = subject_name[1], 
                 temperature = temperature[1],
                 flu_status = flu_status[1],
                 gender = gender[1],
                 blood = blood[1])

# 환자의 데이터 출력
subject1

## 리스트를 접근하는 방법들

# 위치로 리스트의 하나 원소 추출
subject1[2]
# 이름으로 리스트의 하나 원소 추출
subject1$temperature

# 이름 벡터를 명시하여 일부 리스트 원소 추출
subject1[c("temperature", "flu_status")]

## 벡터와 같은 리스트 접근
# 4, 5번째 값
subject1[4:5]

## 데이터 프레임(Data frames) -----

# 환자 데이터에 대한 데이터 프레임 생성

pt_data <- data.frame(subject_name, temperature, flu_status, gender,
                      blood, stringsAsFactors = FALSE)

# 데이터 프레임 출력
pt_data

## 데이터 프레임 접근

# 하나의 열 추출
pt_data$subject_name

# 이름 벡터를 명시하여 몇몇 열 추출
pt_data[c("temperature", "flu_status")]

# 위와 동일하게 temperature과 flu_status을 추출
pt_data[2:3]

# 행과 열로 접근
pt_data[1, 2]

# 벡터를 사용하여 일부 행, 열 접근
pt_data[c(1, 3), c(2, 4)]

## 모든 형, 열을 추출할 때, 관련 행과 열을 공란으로 둠

# 1번째 열, 모든 행
pt_data[, 1]
# 1번째 행, 모든 열
pt_data[1, ]
# 모든 행, 열
pt_data[ , ]

# 다음은 위와 같음
pt_data[c(1, 3), c("temperature", "gender")]
pt_data[-2, c(-1, -3, -5)]

## 매트릭스(Matrixes) -----

# 2x2 매트릭스 생성
m <- matrix(c('a', 'b', 'c', 'd'), nrow = 2)
m

# 위와 동일
m <- matrix(c('a', 'b', 'c', 'd'), ncol = 2)
m

# 2x3 매트릭스 생성
m <- matrix(c('a', 'b', 'c', 'd', 'e', 'f'), nrow = 2)
m

# 3x2 매트릭스 생성
m <- matrix(c('a', 'b', 'c', 'd', 'e', 'f'), ncol = 2)
m

# 매트릭스 원소 추출
m[1, 1]
m[3, 2]

# 행 추출
m[1, ]

# 열 추출
m[, 1]

##### 데이터 이해와 처리 --------------------

## 중고차 데이터를 사용한 데이터 처리 예제
usedcars <- read.csv("usedcars.csv", stringsAsFactors = FALSE)

# 중고차 데이터 구조
str(usedcars)

## 수치 변수 처리 -----

# 수치 변수 요약
summary(usedcars$year)
summary(usedcars[c("price", "mileage")])

# 평균 수입 계산
(36000 + 44000 + 56000) / 3
mean(c(36000, 44000, 56000))

# 수입 중간값
median(c(36000, 44000, 56000))

# 중고차 가격의 최소/최대값
range(usedcars$price)

# 범위의 차
diff(range(usedcars$price))

# 중고차 가격의 IQR
IQR(usedcars$price)

# 요약 계산을 위한 quantile 사용
quantile(usedcars$price)

# 99번째 percentile
quantile(usedcars$price, probs = c(0.01, 0.99))

# quintiles
quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20))

# 중고차 가격과 마일리지의 boxplot
boxplot(usedcars$price, main="Boxplot of Used Car Prices",
      ylab="Price ($)")

boxplot(usedcars$mileage, main="Boxplot of Used Car Mileage",
      ylab="Odometer (mi.)")

# 중고차 가격과 마일리지의 히스토그램
hist(usedcars$price, main = "Histogram of Used Car Prices",
     xlab = "Price ($)")

hist(usedcars$mileage, main = "Histogram of Used Car Mileage",
     xlab = "Odometer (mi.)")

# 중고차의 분산과 표준편차
var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)

## 수치 변수 처리 -----

# 중고차 데이터에 대한 one-way tables
table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

# 데이블 분포 계산
model_table <- table(usedcars$model)
prop.table(model_table)

# 데이터 반올림
color_table <- table(usedcars$color)
color_pct <- prop.table(color_table) * 100
round(color_pct, digits = 1)

## 변수간의 관계 처리 -----

# 가격과 마일리지의 scatterplot
plot(x = usedcars$mileage, y = usedcars$price,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Used Car Odometer (mi.)",
     ylab = "Used Car Price ($)")

# 보수적인 색을 나타내는 새로운 변수
usedcars$conservative <-
  usedcars$color %in% c("Black", "Gray", "Silver", "White")

# 변수 확인
table(usedcars$conservative)

# 모델의 보수적인 색에 대한 Crosstab
CrossTable(x = usedcars$model, y = usedcars$conservative)
