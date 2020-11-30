# R에서 RDS 파일 사용하기

# RDS 파일은 R 전용 데이터 파일로 다른 파일들에 비해 R에서 읽고 쓰는 속도가 빠르고 용량이 작다는 장점이 있습니다. 
# 일반적으로 R에서 분석 작업을 할때는 RDS파일을 이용하고, R을 사용하지 앟는 사람과 파일을 주고받을 때는 CSV 파일을 이용합니다.

# 0. 데이터 불러오기 -------------------------------------------------------------

library(readr)

system.time(nsmc_ratings <- read_tsv("d:/github/data/nsmc/ratings.txt")) # ratings.txt : 18.6MB
## 사용자  시스템 elapsed 
## 0.16    0.00    0.16 

# 1. RDS 파일로 저장하기 ---------------------------------------------------------

saveRDS(nsmc_ratings, file="d:/github/data/nsmc/nsmc_ratings.rds")


# 2. RDS 파일 불러오기 ----------------------------------------------------------

system.time(tets_readRDS <-readRDS("d:/github/data/nsmc/nsmc_ratings.rds")) # nsmc_ratings.rds : 8MB
## 사용자  시스템 elapsed 
## 0.24    0.00    0.24