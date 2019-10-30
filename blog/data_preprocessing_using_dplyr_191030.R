#-------------------------------------------------
# Data preprocessing using dplyr
# (https://rpubs.com/jmhome/R_data_wrangling)
#
# Date : 2019-10-30
#-------------------------------------------------


# 1. 함수 소개 ----------------------------------------------------------------
# 1) filter() : 행 추출
# 2) select() : 열(변수) 추출
# 3) arrange() : 정렬
# 4) mutate() : 변수 추가
# 5) summarise() : 통계치 산출
# 6) group_by() : 집단별로 나누기
# 7) left_join() : 데이터 합치기(열)
# 8) bind_rows() : 데이터 합치기(행)


# 2. 데이터 추출하기 -------------------------------------------------------------

library(dplyr)

# 2.1 filter() ------------------------------------------------------------

# 데이터 불러오기
data(iris)

# Sepal.Length 가 5 이하인 값 선택
iris %>% filter(Sepal.Length <= 5.0)

# Sepal.Length 가 5 미만인 값 선택
iris %>% filter(Sepal.Length < 5.0)

# Species가 setosa인 값 선택
iris %>% filter(Species == "setosa")

# Species가 setosa가 아닌 값 선택
iris %>% filter(Species != "setosa")

# [다중 조건 : AND] Sepal.Length 가 5.0 이하면서, Petal.Width가 1 이상인 값 선택
iris %>% filter(Sepal.Length <= 5.0 & Petal.Width >= 1)

# [다중 조건 : AND] Sepal.Length 가 5.0 이하면서, Petal.Width가 1 이상인 virginica 선택
iris %>% filter(Sepal.Length <= 5.0 & Petal.Width >= 1 & Species == "virginica")

# [다중 조건 : OR] Sepal.Length 가 5.0 이하거나, Petal.Width가 1 이상인 값 선택
iris %>% filter(Sepal.Length <= 5.0 | Petal.Width >= 1)

# [다중 조건 : %in%] Sepal.Length 가 5.0 또는 4.8인 값 선택
iris %>% filter(Sepal.Length %in% c(5.0, 4.8))
# 동일 : iris %>% filter(Sepal.Length == 5.0 | Sepal.Length == 4.8)

# [다중 조건 : %in%] Species가 Setosa 
iris %>% filter(Species %in% c("setosa", "versicolor"))
# 동일 : iris %>% filter(Species == "setosa" | Species == "versicolor")


# 2.2 Select() ------------------------------------------------------------

# Species 컬럼 선택
iris %>% select(Species)

# Sepal.Length와 Species 컬럼 선택
iris %>% select(Sepal.Length, Species)

# Species 컬럼 제외
iris %>% select(-Species)

# Sepal.Length와 Species 컬럼 제외
iris %>% select(-c(Sepal.Length, Species))

# '%>%'를 활용한 처리
iris %>% 
  filter(Species == "setosa") %>% 
  select(Petal.Width)

iris %>% 
  filter(Species == "setosa") %>% 
  filter(Petal.Width >= 0.4) %>% 
  select(Petal.Width)


# 2.3 arrange() -----------------------------------------------------------

# 오름차순 정렬
iris %>% arrange(Petal.Width) %>% head()

# 내림차순 정렬 : desc() 옵션 사용
iris %>% arrange(desc(Petal.Width)) %>% head()

# 멀티 정렬
iris %>% arrange(Petal.Width, desc(Petal.Length)) %>% head(10)


# 2.4 mutate() ------------------------------------------------------------

# 총합 추가
iris %>% 
  mutate(sum = Sepal.Length + Sepal.Width + Petal.Length + Petal.Width) %>% 
  head()

# 총합, 평균 추가
iris %>% 
  mutate(sum = Sepal.Length + Sepal.Width + Petal.Length + Petal.Width) %>% 
  mutate(avg = sum/4) %>% 
  head()

# ifelse() 활용
iris %>% 
  mutate(sum = Sepal.Length + Sepal.Width + Petal.Length + Petal.Width) %>% 
  mutate(avg = sum/4) %>% 
  mutate(avg = ifelse(sum > 10, "10+", "")) %>% 
  head()


# 2.5 summarise() ---------------------------------------------------------

# 요약하기
iris %>% 
  summarize(avg_SepalLength = mean(Sepal.Length))


# 2.6 group_by() ----------------------------------------------------------

# 그룹 요약하기
iris %>% 
  group_by(Species) %>% 
  summarize(avg_SepalLength = mean(Sepal.Length))


