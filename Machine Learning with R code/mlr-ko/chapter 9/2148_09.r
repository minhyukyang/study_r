##### 9장 : k-평균(k-mean)을 활용한 군집화 -------------------

## 예제 : 10대 시장 찾기 ----
## 2단계 : 데이터 준비와 살펴보기 ----
teens <- read.csv("snsdata.csv")
str(teens)

# female 변수의 결측 데이터 확인
table(teens$gender)
table(teens$gender, useNA = "ifany")

# age 변수의 결측 데이터 확인
summary(teens$age)

# age 이상치(outliers) 제거
teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                     teens$age, NA)

summary(teens$age)

# "unknown"인 성별값에 재부여
teens$female <- ifelse(teens$gender == "F" &
                         !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

# 재지정한 작업에 대한 확인
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

# 집단(cohort)별 나이 평균
mean(teens$age) # doesn't work
mean(teens$age, na.rm = TRUE) 

# 집단별 나이
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

# 각 개인에 대한 예측된 나이 계산
ave_age <- ave(teens$age, teens$gradyear,
                 FUN = function(x) mean(x, na.rm = TRUE))


teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

# 제거한 결측치에 대한 요약 결과 확인
summary(teens$age)

## 3단계 : 데이터로 모델 훈련 ----
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))

teen_clusters <- kmeans(interests_z, 5)

## 4단계 : 모델 성능 평가 ----
# 군집의 크기 확인
teen_clusters$size

# 군집의 중앙점(centers) 확인
teen_clusters$centers

## 5단계 : 모델 성능 향상 ----
# 본래 데이터 프레임에 군집ID(cluster ID) 적용
teens$cluster <- teen_clusters$cluster

# 처음 5개 데이터 확인
teens[1:5, c("cluster", "gender", "age", "friends")]

# 군집별 평균 나이
aggregate(data = teens, age ~ cluster, mean)

# 군집별 여성 비율
aggregate(data = teens, female ~ cluster, mean)

# 군집별 친구 수의 평균
aggregate(data = teens, friends ~ cluster, mean)
