# setwd("D:/Analysis/M_201904")
setwd("D:/Analysis/test_mdb/")

### 1. 데이터 확인

# 파일에는 총 6개의 테이블이 들어있으며, 암호가 설정되어 있습니다.

### 2. 데이터 로드

# - 패키지 로드 및 설정값 세팅 

library(RODBC)

p_driver <- "{Microsoft Access Driver (*.mdb, *.accdb)};" # 드라이버
p_dbq <- "d:/Analysis/test_mdb/test.mdb;" # 파일 경로
p_pwd <- "******;" # 패스워드

# for 64 bit windows
odbc_conn <- odbcDriverConnect(paste0("Driver=",p_driver,"DBQ=",p_dbq,"pwd=",p_pwd))
odbc_conn

# - 전체 테이블 정보 가져오기

mdb_info <- sqlTables(odbc_conn)
mdb_info

# - 테이블명 추출

library(dplyr)

tableList <- mdb_info %>% filter(TABLE_TYPE == "TABLE") %>% select("TABLE_NAME")
tableList <- tableList[,1]
tableList

### 3. 테이블별 csv 파일 생성

# - `for`문을 이용한 반복 처리

library(xlsx)

target_dir <- "./output/"

for(i in 1:length(tableList)){
  
  fname <- paste0(tableList[i],".csv") # 테이블명과 동일한 파일명 생성
  tmp <- sqlFetch(odbc_conn, tableList[i]) # 테이블내 데이터 가져오기
  write.csv(tmp, file = paste0(target_dir,fname), row.names = FALSE) # csv 파일로 저장 
  
  print(paste0(target_dir,fname)) # 결과물 출력
}
