# Title : 텍스트 분석을 위한 R
# Sub-Title : Tidytext와 형태소 분석기를 이용한 텍스트 마이닝 기초
# Author : 박찬엽(mrchypark)
# Link : https://mrchypark.github.io/textR/print.html

# 1. 텍스트 관련 R 패키지 설치 가이드 --------------------------------------------------

# 1.1 windows 사용자 ---------------------------------------------------------

# 1.1.1 jdk 8 설치 ----------------------------------------------------------

# jdk8 설치
# https://www.oracle.com/technetwork/java/javase/downloads/index.html

# 1.1.2 KoNLP 패키지 설치 및 테스트 ------------------------------------------------

if (!requireNamespace("KoNLP")) {
  install.packages("KoNLP", repos = "https://cloud.r-project.org")
}
library(KoNLP)

test <- "한글 테스트 입니다."
# 아래 결과가 나와야 합니다.
extractNoun(test)
#> [1] "한글"   "테스트"

# 1.1.3 RcppMeCab 패키지 설치 및 테스트 --------------------------------------------

if (!requireNamespace("RcppMeCab")) {
  install.packages("RcppMeCab", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("RmecabKo")) {
  install.packages("RmecabKo", repos = "https://cloud.r-project.org")
}
#> Loading required namespace: RmecabKo
# c:에 권한이 없다면 "d:/mecab"으로 설정
RmecabKo::install_mecab("c:/mecab")
library(RcppMeCab)
test <- "한글 테스트 입니다."
# 아래 결과가 나와야 합니다.
pos(test)
# $`\xc7ѱ\xdb \xc5\u05fd�Ʈ \xc0Դϴ\xd9.`
# [1] "\xc7\xd1/SL" "�\xdb /SY"  "\xc5\xd7/SL" "��/SY"     "Ʈ/SL"        "\xc0Դ/SY"    "ϴ/SL"        "\xd9./SY"   

# 한글이 깨질 경우 아래 사용
library(RcppMeCab)
test <- "한글 테스트 입니다."
# iconv 함수는 인코딩을 변경하는 함수입니다.
test <- iconv(test, to = "UTF-8")
pos(test)
# $`한글 테스트 입니다.`
# [1] "한글/NNG"      "테스트/NNG"    "입니다/VCP+EF" "./SF"     


# 1.2 macOS 사용자 -----------------------------------------------------------

# 1.2.1 터미널에서 설치해야 하는 도구들 -------------------------------------------------

# 1.2.1.1 JDK8 ------------------------------------------------------------

# https://www.youtube.com/watch?v=v8xZWbIASc0

# 1.2.1.2 mecab-ko --------------------------------------------------------

# https://bitbucket.org/eunjeon/mecab-ko-dic

# 1.2.1.3 3. mecab-ko-dic -------------------------------------------------

# https://bitbucket.org/eunjeon/mecab-ko-dic


# 1.2.2 KoNLP 패키지 설치 및 테스트 ------------------------------------------------

if (!requireNamespace("KoNLP")) {
  install.packages("KoNLP", repos = "https://cloud.r-project.org")
}
library(KoNLP)

test <- "한글 테스트 입니다."
extractNoun(test)
#> [1] "한글"   "테스트"


# 1.2.3 RcppMeCab 패키지 설치 및 테스트 --------------------------------------------

if (!requireNamespace("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("RcppMeCab")) {
  remotes::install_github("junhewk/RcppMeCab")
}
library(RcppMeCab)

test <- "한글 테스트 입니다."
pos(test)
#> $`<c7><U+0471><db> <c5><U+05FD><U+00BA><U+01AE> <c0><U+0534><U+03F4><d9>.`
#> [1] "<c7><d1>/SL" "<U+00B1><db> /SY"
#> [3] "<c5><d7>/SL" "<U+00BD><U+00BA>/SY"
#> [5] "<U+01AE>/SL" "<c0><U+0534>/SY"
#> [7] "<U+03F4>/SL" "<d9>./SY"

# 만약에 글자가 깨진다면 iconv() 함수를 사용해 보세요.

# iconv 함수는 인코딩을 변경하는 함수입니다.
test <- iconv(test, to = "UTF-8")
pos(test)
#> $`한글 테스트 입니다.`
#> [1] "한글/NNG"      "테스트/NNG"   
#> [3] "입니다/VCP+EF" "./SF"

# 1.3 공통 패키지 설치 -----------------------------------------------------------

# remotes 패키지는 패키지 인스톨을 위한 함수를 제공합니다. 
# tidytext 패키지는 텍스트를 tidy하게 다룰 수 있게 해줍니다. 
# presidentSpeech 패키지는 역대 대통령 연설문 텍스트를 사용할 수 있게 해주는 패키지입니다.

if (!requireNamespace("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("tidytext")) {
  install.packages("tidytext", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("presidentSpeech")) {
  remotes::install_github("forkonlp/presidentSpeech")
}

library(tidytext)
library(presidentSpeech)


# 2. 사전 지식 ----------------------------------------------------------------

# 2.1 tidyverse -----------------------------------------------------------

# What is tidyverse?
# 1. RStudio가 개발, 관리하는 패키지
# 2. 공식 문서가 매우 잘 되어 있음
# 3. 사용자층이 두터워 영어로 검색하면 많은 질답을 찾을 수 있음
# 4. 커뮤니티 설명글도 매우 많음
# 5. 6개의 핵심 패키지 포함 23가지 패키지로 이루어진 메타 패키지
# 6. tidy data 라는 사상과 파이프 연산자로 대동단결
# 7. 사상에 영감을 받아 맞춰서 제작하는 개인 패키지가 많음(ex> tidyquant, tidytext 등)

if (!requireNamespace("tidyverse")) {
  install.packages("tidyverse")}
library(tidyverse)

# 2.1.1 파이프 연산자(%>%) ------------------------------------------------------

# 함수를 중첩해서 사용할 일이 점점 빈번해 짐
plot(diff(log(sample(rnorm(10000, mean = 10, sd = 1), size = 100, replace = FALSE))), col = "red", type = "l")

# %>%를 사용하면
# - 생각의 순서대로 함수를 작성할 수 있음
# - 중간 변수 저장을 할 필요가 없음
# - 순서가 읽이 용이하여 기억하기 좋음

rnorm(10000,mean=10,sd=1) %>%
  sample(size=100,replace=FALSE) %>%
  log %>%
  diff %>%
  plot(col="red",type="l")

# flights 데이터에 파이프 연산자 사용예 1
if (!requireNamespace("nycflights13")) {
  install.packages("nycflights13")}
library(nycflights13)

flights %>%
  group_by(year,month,day) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE))
#> # A tibble: 365 x 4
#> # Groups:   year, month [12]
#>    year month   day delay
#>   <int> <int> <int> <dbl>
#> 1  2013     1     1 11.5 
#> 2  2013     1     2 13.9 
#> 3  2013     1     3 11.0 
#> 4  2013     1     4  8.95
#> 5  2013     1     5  5.73
#> 6  2013     1     6  7.15
#> # ... with 359 more rows

# group_by()는 filter()와도 함께 사용할 수 있음

flights %>% 
  group_by(dest) %>% 
  filter(n() > 365) -> 
  popular_dests
popular_dests
# # A tibble: 332,577 x 19
# # Groups:   dest [77]
# year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time arr_delay carrier flight tailnum origin dest  air_time distance  hour minute
# <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>     <dbl> <chr>    <int> <chr>   <chr>  <chr>    <dbl>    <dbl> <dbl>  <dbl>
#   1  2013     1     1      517            515         2      830            819        11 UA        1545 N14228  EWR    IAH        227     1400     5     15
# 2  2013     1     1      533            529         4      850            830        20 UA        1714 N24211  LGA    IAH        227     1416     5     29
# 3  2013     1     1      542            540         2      923            850        33 AA        1141 N619AA  JFK    MIA        160     1089     5     40
# 4  2013     1     1      544            545        -1     1004           1022       -18 B6         725 N804JB  JFK    BQN        183     1576     5     45
# 5  2013     1     1      554            600        -6      812            837       -25 DL         461 N668DN  LGA    ATL        116      762     6      0
# 6  2013     1     1      554            558        -4      740            728        12 UA        1696 N39463  EWR    ORD        150      719     5     58
# 7  2013     1     1      555            600        -5      913            854        19 B6         507 N516JB  EWR    FLL        158     1065     6      0
# 8  2013     1     1      557            600        -3      709            723       -14 EV        5708 N829AS  LGA    IAD         53      229     6      0
# 9  2013     1     1      557            600        -3      838            846        -8 B6          79 N593JB  JFK    MCO        140      944     6      0
# 10  2013     1     1      558            600        -2      753            745         8 AA         301 N3ALAA  LGA    ORD        138      733     6      0
# # ... with 332,567 more rows, and 1 more variable: time_hour <dttm>

# 사용할 데이터부터 순서대로 함수를 작성할 수 있는 장점

popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)
#> # A tibble: 131,106 x 6
#> # Groups:   dest [77]
#>    year month   day dest  arr_delay
#>   <int> <int> <int> <chr>     <dbl>
#> 1  2013     1     1 IAH          11
#> 2  2013     1     1 IAH          20
#> 3  2013     1     1 MIA          33
#> 4  2013     1     1 ORD          12
#> 5  2013     1     1 FLL          19
#> 6  2013     1     1 ORD           8
#> # ... with 1.311e+05 more rows, and 1
#> #   more variable: prop_delay <dbl>

# 2.1.2 단정한 데이터(tidy data) ------------------------------------------------

# 1. Hadley Wickham (https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html)
# 1.1 Each variable forms a column.
# 1.2 각 변수는 개별의 열(column)으로 존재한다.
# 1.3 각 열에는 개별 속성이 들어간다.

# 2. 고감자님의 블로그 (http://freesearch.pe.kr/archives/3942)
# 2.1 Each observation forms a row.
# 2.2 각 관측치는 행(row)를 구성한다.
# 2.3 각 행에는 개별 관찰 항목이 들어간다.

# 3. 헬로데이터과학 (http://www.hellodatascience.com/?p=287)
# 3.1 Each type of observational unit forms a table.
# 3.2 각 테이블은 단 하나의 관측기준에 의해서 조직된 데이터를 저장한다.
# 3.3 각 테이블에는 단일 유형의 데이터가 들어간다.

# 참고 : Garrett Grolemund의 Data Science with R (http://garrettgman.github.io/tidying/)

# 2.1.3 long form과 wide form ----------------------------------------------

# long form
# 컴퓨터가 계산하기 좋은 모양
# tidy data의 요건을 충족
# tidyverse의 패키지 대부분의 입력 형태

# wide form
# 사람이 눈으로 보기 좋은 모양
# 2개 변수에 대한 값만 확인 가능
# dashboard 형이라고도 하며 조인 등 연산이 어려움


# 2.2 데이터 패키지(presidentSpeech) 소개

# 대통령 기록 연구실의 대통령 연설문을 제공
# 패키지 설명 : https://forkonlp.github.io/presidentSpeechKr/

if (!requireNamespace("presidentSpeech")) {
  remotes::install_github("forkonlp/presidentSpeech")
}
library(presidentSpeech)

# 2.2.1 조건 확인 함수 ----------------------------------------------------------

# 대통령 조건 확인
get_president()
# [1] "이승만" "윤보선" "박정희" "최규하" "전두환" "노태우" "김영삼" "김대중" "노무현" "이명박"

# 연설 분야 조건 확인
get_field()
# [1] "국정전반"       "정치/사회"      "산업/경제"      "외교/통상"      "국방"           "과학기술정보"   "교육"           "문화/체육/관광" "환경"           "기타"

# 연설 유형 확인
get_event()
# [1] "취임사"      "신년사"      "국회연설"    "기념사"      "만찬사"      "환영사"      "치사"        "성명/담화문" "라디오연설"  "기타"      

# 연설 리스트 데이터
library(dplyr)
data(spidx)
glimpse(spidx)
# Observations: 6,681
# Variables: 6
# $ president <chr> "이승만", "이승만", "이승만", "이승만", "이승만", "이승만", "이승만", "이승만", "이승만", "이승만", "이승만", "이승만", "이승만", "이승만", "이승만", "이승만", "이승만", "이승만", "이승만", "이승만", "이승만", "이승만...
# $ field     <chr> "기타", "국정전반", "정치/사회", "국정전반", "정치/사회", "정치/사회", "국정전반", "정치/사회", "정치/사회", "정치/사회", "정치/사회", "산업/경제", "국정전반", "기타", "외교/통상", "외교/통상", "국방", "국방", "국정전반...
# $ event     <chr> "성명/담화문", "취임사", "성명/담화문", "기타", "성명/담화문", "성명/담화문", "기념사", "성명/담화문", "성명/담화문", "성명/담화문", "성명/담화문", "성명/담화문", "기념사", "성명/담화문", "성명/담화문", "성명/담화문", "기타"...
# $ title     <chr> "학생제군에게", "대통령 취임사(大統領就任辭) ", "민족이 원하는 길을 따를 결심, 국무총리 인준 부결에 대하여 ", "미급점(未及點) 육성하라 ", "전민족에게 충고함 ", "대한민국 정부수립과 우리의 각오 ", "함께 뭉쳐서 자강전진(自疆前進) 외모(外侮)막자 (...
# $ date      <chr> "1948", "1948.07.24", "1948.07.29", "1948.08.09", "1948.08.12", "1948.08.15", "1948.08.15", "1948.09.04", "1948.09.05", "1948.09.22", "1948.09.24", "19...
# $ link      <chr> "http://www.pa.go.kr/research/contents/speech/index.jsp?spMode=view&catid=c_pa02062&artid=1310475", "http://www.pa.go.kr/research/contents/speech/index...

# 데이터를 사용한 필터링 예시
spidx %>% 
  filter(president == "윤보선")
# A tibble: 3 x 6
# president field    event  title                                       date       link                                                                                       
# <chr>     <chr>    <chr>  <chr>                                       <chr>      <chr>                                                                                      
#   1 윤보선    국정전반 취임사 제2대 윤보선 대통령 취임사                  1960.08.13 http://www.pa.go.kr/research/contents/speech/index.jsp?spMode=view&catid=c_pa02062&artid=1~
#   2 윤보선    기타     기타   "윤보선 대통령 부산연설 \"독재재기 경계\""  1960.08.29 http://www.pa.go.kr/research/contents/speech/index.jsp?spMode=view&catid=c_pa02062&artid=1~
#   3 윤보선    기타     기타   "윤보선 대통령 대구연설 \"도의심 앙양 강조\""~ 1960.09.15 http://www.pa.go.kr/research/contents/speech/index.jsp?spMode=view&catid=c_pa02062&artid=1~

# 2.2.2 연설문 텍스트 가져오기 ------------------------------------------------------

spidx %>% 
  filter(president == "윤보선") %>% 
  top_n(1, wt = desc(date)) %>% 
  pull(link) -> 
  tar
get_speech(tar)
# A tibble: 1 x 9
# title             date     president place field   event source paragraph content                                                                                           
# <chr>             <chr>    <chr>     <chr> <chr>   <chr> <chr>      <int> <chr>                                                                                             
#   1 제2대 윤보선 대통령 취임사~ 1960.08~ 윤보선    국내  국정전반~ 취임사~ ""             1 "제2공화국의 초대대통령으로 영예의 당선을 얻은 어제 나의 감격은 선서식을 거행하는 오늘에는 영광된 의무감과 무거운 책임감으로 변해 졌읍니다. 비록 엄숙 해야 할 식전 이기는~

# 2.2.3 연습문제 --------------------------------------------------------------

# 1. presidentSpeech 패키지에서 검색할 수 있는 대통령은 총 몇명인가요?
get_president() %>% 
  length
  
# 2. 윤보선 대통령과 박정희 대통령은 각각 몇 개의 연설문이 있나요?
spidx %>% 
  mutate(cnt = 1) %>%
  filter(president %in% c("윤보선", "박정희")) %>%
  select(president, cnt) %>%
  group_by(president) %>%
  summarise(freq=sum(cnt))

# 3. nchar() 함수는 글자수를 세주는 함수입니다. 최규하 대통령의 취임사는 총 몇 글자 인가요?
spidx %>% 
  filter(president == "최규하", event == "취임사") %>%
  pull(link) %>% 
  get_speech() %>%
  pull(content) %>% 
  nchar()

# 3. 단정하게 텍스트를 다루는 tidytext ------------------------------------------------

# 3.1 tidy text data ------------------------------------------------------

## policy:
# - a table with one-token-per-row
# - 한 행(row)에 한 토큰(token)으로 테이블을 구성해야 한다.

## token?
# - 글자 중 의미를 가진 단위를 총칭.
# - tokenization은 가지고 있는 텍스트 자원을 token 단위로 나누는 것을 뜻함.
# ex> 자소(자음, 모음), 음소(글자), 형태소, 단어, n-gram 등

# 3.2 tidytext 패키지 --------------------------------------------------------

## tidytext (https://juliasilge.github.io/tidytext/)
# - 한 행(row)에 한 토큰(token)으로 테이블을 구성하기 위한 패키지
# - 파이프 연산자를 지원
# - 여러 가지 token과 tm 패키지와의 호환 기능을 제공
# - 자세히 소개하는 온라인 사이트(영문) : https://www.tidytextmining.com/

if (!requireNamespace("tidytext")) {
  install.packages("tidytext")
}
library(tidytext)

# 3.2.1 token 단위 처리 unnest_tokens() ---------------------------------------

# 기본값인 단어 단위(특수문자 제거, 띄어쓰기 기준) token으로 동작.
# unnest_tokens() 함수는 텍스트 데이터를 token 단위로 풀어내는 동작을 수행

## Function : uunnest_tokens()
# unnest_tokens(
#   # 다루고자 하는 텍스트 데이터 객체
#   tbl = 텍스트 데이터,      
#   # token화의 결과가 작성될 열의 이름
#   output = 결과열의 이름,   
#   # 텍스트 데이터 객체 내의 텍스트 열
#   input = 목표 텍스트 열,   
#   # 기본값(words 단위 = 띄어쓰기 단위)이 있어 생략 가능
#   token = "words",         
#   # 기타 옵션들
#   ...                     
# )

## Sample code
tar
#> [1] "http://www.pa.go.kr/research/contents/speech/index.jsp?spMode=view&catid=c_pa02062&artid=1310437"

# 연설문 중 1개를 가져와서
get_speech(tar) %>%
  # 대통령 컬럼과 연설문 컬럼만 선택한 후
  select(president, content) %>% 
  # 연설문 컬럼을 띄어쓰기 단위로 쪼갠 결과물을 word라는 컬럼으로 출력
  unnest_tokens(
    input = content,
    output = word
  )
# # A tibble: 483 x 2
# president word          
# <chr>     <chr>         
#   1 윤보선    제            
# 2 윤보선    2             
# 3 윤보선    공화국의      
# 4 윤보선    초대대통령으로
# 5 윤보선    영예의        
# 6 윤보선    당선을        
# 7 윤보선    얻은          
# 8 윤보선    어제          
# 9 윤보선    나의          
# 10 윤보선    감격은        
# # ... with 473 more rows

# 3.2.2 연습문제 --------------------------------------------------------------

# 1. 김영삼 대통령의 첫 국무회의 연설문을 띄어쓰기 단위로 자르면 총 몇 단어인가요?

spidx %>% 
  filter(president == "김영삼", grepl("국무회의", title)) %>%
  top_n(1, wt = desc(date)) %>% 
  pull(link) %>% 
  get_speech() %>%
  unnest_tokens(input = content, output = word) %>% 
  nrow()
# [1] 509


# 4. 형태소 분석 ---------------------------------------------------------------

# 4.1 한글의 특징 형태소 ----------------------------------------------------------

# 형태소란 의미를 가지는 최소 단위
# "철수가 밥을 먹었다."

#> $철수가
#>  [1] "철수/ncpa+가/jcc"         
#>  [2] "철수/ncpa+가/jcs"         
#>  [3] "철수/ncpa+가/ncn"         
#>  [4] "철수/ncpa+이/jp+가/ecc"   
#>  [5] "철수/ncn+가/jcc"          
#>  [6] "철수/ncn+가/jcs"          
#>  [7] "철수/ncn+가/ncn"          
#>  [8] "철수/ncn+이/jp+가/ecc"    
#>  [9] "철/xp+수가/ncn"           
#> [10] "철/xp+수/ncn+가/jcc"      
#> [11] "철/xp+수/ncn+가/jcs"      
#> [12] "철/xp+수/ncn+이/jp+가/ecc"
#> 
#> $밥을
#> [1] "밥/ncn+을/jco"  "밥/ncn+을/jcs" 
#> [3] "밥/ncpa+을/jco" "밥/ncpa+을/jcs"
#> [5] "밥/ncps+을/jco" "밥/ncps+을/jcs"
#> 
#> $먹었다
#> [1] "먹/pvg+었/ep+다/ef"
#> 
#> $.
#> [1] "./sf" "./sy"

# 참고 : KAST 품사 태그셋 (https://raw.githubusercontent.com/haven-jeon/KoNLP/master/etcs/figures/konlp_tags.png)

# 4.2 형태소 분석기 -------------------------------------------------------------

# 4.2.1 R의 대표적인 형태소 분석기 ---------------------------------------------------

# # RcppMeCab
# - 일본어 형태소 분석기인 mecab 기반
# - C++ 로 작성하여 속도가 매우 빠름
# - 일본어, 중국어 등도 사용 가능
# - 형태소 분석 함수를 제공
# - 띄어쓰기에 덜 민감함

# # KoNLP
# - 가장 유명한 형태소 분석기
# - java로 작성된 한나눔 분석기 기반
# - 우리샘, NiaDIC 등 자체 사전
# - 텍스트 분석을 위한 기능들을 제공
# - 친절한 설명서

# RcppMeCab 설치 확인
library(RcppMeCab)
pos(iconv("롯데마트가 판매하고 있는 흑마늘 양념 치킨이 논란이 되고 있다.", to = "utf8"))
#> $`롯데마트가 판매하고 있는 흑마늘 양념 치킨이 논란이 되고 있다.`
# [1] "롯데마트/NNP" "가/JKS"       "판매/NNG"     "하/XSV"       "고/EC"        "있/VX"        "는/ETM"       "흑/NNG"       "마늘/NNG"     "양념/NNG"     "치킨/NNG"     "이/JKS"      
# [13] "논란/NNG"     "이/JKS"       "되/VV"        "고/EC"        "있/VX"        "다/EF"        "./SF"        

# KoNLP 설치 확인
library(KoNLP)
SimplePos09("롯데마트가 판매하고 있는 흑마늘 양념 치킨이 논란이 되고 있다.")
#> $롯데마트가
#> [1] "롯데마트/N+가/J"
#> 
#> $판매하고
#> [1] "판매/N+하고/J"
#> 
#> $있는
#> [1] "있/P+는/E"
#> 
#> $흑마늘
#> [1] "흑마늘/N"
#> 
#> $양념
#> [1] "양념/N"
#> 
#> $치킨이
#> [1] "치킨/N+이/J"
#> 
#> $논란이
#> [1] "논란/N+이/J"
#> 
#> $되고
#> [1] "되/P+고/E"
#> 
#> $있다
#> [1] "있/P+다/E"
#> 
#> $.
#> [1] "./S"

# 4.2.2 생각해보기 -------------------------------------------------------------

# 1. 노태우 대통령의 취임사를 RcppMeCab 패키지의 pos() 함수로 형태소 분석한 결과를 출력하세요.
spidx %>% 
  filter(president == "노태우", grepl("취임사", title)) %>%
  top_n(1, wt = desc(date)) %>% 
  pull(link) %>% 
  get_speech() %>%
  pull(content) %>% 
  pos()

# 2. 김대중 대통령의 취임사를 KoNLP 패키지의 SimplePos09() 함수로 형태소 분석한 결과를 출력하세요.
spidx %>% 
  filter(president == "김대중", grepl("취임사", title)) %>%
  top_n(1, wt = desc(date)) %>% 
  pull(link) %>% 
  get_speech() %>%
  pull(content) %>% 
  SimplePos09()

# 4.2.3 형태소로 token화 -------------------------------------------------------

# 4.2.3.1 데이터 준비 ----------------------------------------------------------

library(tidytext)
library(dplyr)
library(presidentSpeech)

spidx %>% 
  filter(president == "이명박") %>% 
  filter(grepl("취임사", title)) %>% 
  pull(link) -> 
  tar

# 연설문 가져와서 word(띄어쓰기) 단위로 나누기

# 연설문 중 1개를 가져와서
get_speech(tar, paragraph = T) %>%
  # 문단 컬럼과 연설문 컬럼만 선택한 후
  select(paragraph, content) %>% 
  # 연설문 컬럼을 word 단위로 쪼갠 결과물을 word라는 컬럼으로 출력
  unnest_tokens(
    input = content,
    output = word
  )
# # A tibble: 2,019 x 2
# paragraph word    
# <int> <chr>   
#   1         1 존경하는
# 2         1 국민    
# 3         1 여러분  
# 4         2 700     
# 5         2 만      
# 6         2 해외동포
# 7         2 여러분  
# 8         3 이      
# 9         3 자리에  
# 10         3 참석하신
# # ... with 2,009 more rows

# # 형태소 분석기와 함께 사용하기
# unnest_tokens(
#   tbl = 텍스트 데이터,      
#   input = 목표 텍스트 열,   
#   output = 결과열의 이름,   
#   token = "words",         <- 여기에 형태소 분석 함수를 적용
#   ...                     
# )

# KoNLP의 SimplePos09() 함수를 활용해서 형태소 단위로 쪼갠 데이터를 만듭니다.
library(KoNLP)
# 연설문 중 1개를 가져와서
get_speech(tar, paragraph = T) %>%
  # 문단 컬럼과 연설문 컬럼만 선택한 후
  select(paragraph, content) %>% 
  # 연설문 컬럼을 형태소 단위로 쪼개 
  # pos라는 컬럼으로 출력
  unnest_tokens(pos, content, token = SimplePos09) %>% 
  # pos 결과물의 순서 보장을 위해 순서 값을 추가
  mutate(pos_order = 1:n()) -> 
  pos_res

pos_res
# # A tibble: 2,216 x 3
# paragraph pos              pos_order
# <int> <chr>                <int>
#   1         1 존경/n+하/x+는/e         1
# 2         1 국민/n                   2
# 3         1 여러분/n                 3
# 4         1 !/s                      4
# 5         2 700/n+만/j               5
# 6         2 해외동포/n               6
# 7         2 여러분/n                 7
# 8         2 !/s                      8
# 9         3 이/m                     9
# 10         3 자리/n+에/j             10
# # ... with 2,206 more rows

# 4.2.3.2 형태소 분석기가 사용하는 태그

# https://raw.githubusercontent.com/haven-jeon/KoNLP/master/etcs/figures/konlp_tags.png

# 4.2.4 불용어 제거 ------------------------------------------------------------

# 4.2.4.1 필요한 형태소 정보만 선택 --------------------------------------------------

# 신뢰할 수 있는 stop word 사전 등이 없기 때문에, 형태소 분석 후 필요한 형태소만 활용.

pos_res %>%
  # 우선 `filter()` 와 `grepl()` 함수를 활용하여 명사(n)만 추출
  filter(grepl("/n", pos)) %>% 
  # 형태소 정보를 제거
  mutate(pos_done = gsub("/.*$", "", pos)) -> 
  n_done

n_done
# # A tibble: 1,326 x 4
# paragraph pos                           pos_order pos_done                   
# <int> <chr>                             <int> <chr>                      
#   1         1 존경/n+하/x+는/e                      1 존경                       
# 2         1 국민/n                                2 국민                       
# 3         1 여러분/n                              3 여러분                     
# 4         2 700/n+만/j                            5 700                        
# 5         2 해외동포/n                            6 해외동포                   
# 6         2 여러분/n                              7 여러분                     
# 7         3 자리/n+에/j                          10 자리                       
# 8         3 참석/n+하/x+시ㄴ/e                   11 참석                       
# 9         3 노무현ᆞ김대중ᆞ김영삼ᆞ전두환/n        12 노무현ᆞ김대중ᆞ김영삼ᆞ전두환
# 10         3 전/n                                 13 전                         
# # ... with 1,316 more rows

# 4.2.4.2 명사, 형용사, 동사 가져오기 ------------------------------------------------

# 명사는 n, 동사/형용사는 p로 표시. 형태소 분석 후 한 글자는 전후 맥락 없이 의미를 파악하기 어렵기 때문에 제거

pos_res %>%
  filter(grepl("/p", pos)) %>% 
  mutate(pos_done = gsub("/.*$", "다", pos)) ->
  p_done

bind_rows(n_done, p_done) %>% 
  arrange(pos_order) %>% 
  filter(nchar(pos_done) > 1) %>% 
  select(paragraph, pos_done) ->
  pos_done

pos_done
# # A tibble: 1,648 x 2
# paragraph pos_done                   
# <int> <chr>                      
#   1         1 존경                       
# 2         1 국민                       
# 3         1 여러분                     
# 4         2 700                        
# 5         2 해외동포                   
# 6         2 여러분                     
# 7         3 자리                       
# 8         3 참석                       
# 9         3 노무현ᆞ김대중ᆞ김영삼ᆞ전두환
# 10         3 대통령                     
# # ... with 1,638 more rows

# 4.2.4.3 함께 사용한 함수 설명 ----------------------------------------------------

# # grep(), grepl()
# - grep() 함수는 글자 데이터내에 찾고자 하는 글자가 있는 위치를 인덱스(숫자)로 알려줌
# - grepl() 함수는 결과를 T/F로 알려줌

# grep(
#   pattern = 찾고자 하는 글자,  
#   x = 글자 데이터,             
#   fixed  = 정규식을 사용 여부, # T/F로 되어 있으며 FALSE가 기본값
#   ...                     
# )

# # gsub()
# - gsub() 함수는 찾고자 하는 글자를 원하는 글자로 바꿔줌

# gsub(
#   pattern = 찾고자 하는 글자,
#   replacement = 찾은 글자가 바뀌게 될 글자,
#   x = 글자 데이터,
#   fixed  = 정규식을 사용 여부, # T/F로 되어 있으며 FALSE가 기본값
#   ...                     
# )

# # nchar()
# - nchar()는 글자 데이터를 받아서 글자수를 알려줌

# nchar(
#   x = 세고자 하는 글자,
#   
#   ...
# )

# 4.2.5 정규 표현식 ------------------------------------------------------------

# # 글자를 다루는데 유용한 기능을 제공
# - ^ : 이걸로 시작함
# - $ : 이걸로 끝남
# - . : 임의의 글자 하나
# - ? : 앞에 있는 문자가 없거나 하나
# - + : 앞에 있는 문자가 하나 이상
# - * : 앞에 있는 문자가 없거나 하나 이상

# https://mrchypark.github.io/dabrp_classnote3/class4#1

# 4.2.6 연습문제 --------------------------------------------------------------

# 1. 아래 코드로 이명박 대통령 연설문 중 10개 를 가져오세요.
# 2. grepl() 함수를 사용해서 제목(title 컬럼)에 나눔이 있는 연설문을 찾으세요.
# 3. gsub() 함수를 사용해서 제목(title 컬럼)에 인터넷 연설 글자를 없애보세요.
# 4. mutate() 함수를 사용해서 연설문id를 1~10까지 id 컬럼으로 추가하세요.
# 5. id와 content 컬럼만 선택하세요.
# 6. 형태소 분석을 하여 결과를 pos 컬럼으로 추가하세요.
# 7. grepl() 함수를 사용해서 명사만 남겨보세요.
# 8. gsub() 함수를 사용해서 POS 정보를 지우고 한글만 남기세요.

library(presidentSpeech)
library(magrittr)
library(tidyverse)

try_speech <- insistently(get_speech)

spidx %>% 
  filter(president == "이명박") %>% 
  arrange(date) %>% 
  top_n(10) %$% 
  map_dfr(link, try_speech) ->
  tar

#> Selecting by link
#> # A tibble: 10 x 9
#>   title date  president place field
#>   <chr> <chr> <chr>     <chr> <chr>
#> 1 제98차~ ""    이명박    국내  외교/통~
#> 2 제99차~ ""    이명박    국내  기타 
#> 3 제100~ ""    이명박    국내  국정전반~
#> 4 제101~ ""    이명박    국내  외교/통~
#> 5 제102~ ""    이명박    국내  과학기술~
#> 6 제103~ ""    이명박    국내  외교/통~
#> # ... with 4 more rows, and 4 more
#> #   variables: event <chr>,
#> #   source <chr>, paragraph <int>,
#> #   content <chr>

# 2. grepl() 함수를 사용해서 제목(title 컬럼)에 나눔이 있는 연설문을 찾으세요.
tar %>%
  filter(grepl("나눔", title))

# 3. gsub() 함수를 사용해서 제목(title 컬럼)에 인터넷 연설 글자를 없애보세요.
tar %>%
  mutate(title=gsub("인터넷 연설","", title))

# 4. mutate() 함수를 사용해서 연설문id를 1~10까지 id 컬럼으로 추가하세요.
tar %>%
  mutate(id=1:10)

# 5. id와 content 컬럼만 선택하세요.
tar %>%
  mutate(id=1:10) %>%
  select(id, content)

# 6. 형태소 분석을 하여 결과를 pos 컬럼으로 추가하세요.
tar %>%
  mutate(id=1:10) %>%
  select(id, content) %>%
  mutate(pos = pos(content))

# 7. grepl() 함수를 사용해서 명사만 남겨보세요.
tar %>%
  mutate(id=1:10) %>%
  select(id, content) %>%
  mutate(pos = pos(content)) %>%
  mutate(pos_noun = filter(grepl("/n", pos)))

# 8. gsub() 함수를 사용해서 POS 정보를 지우고 한글만 남기세요.


  


4.2.7 RcppMeCab 실습
5 텍스트 마이닝 지표
5.1 단어 출현 빈도
5.1.1 단어 출현 빈도 계산
5.1.2 사용예 : 워드클라우드
5.1.3 연습문제
5.2 동시 출현 빈도
5.2.1 동시 출현 빈도 계산
5.2.2 기준 단어로 데이터 탐색
5.2.3 연습문제
5.2.4 사용예 : 네트워크 시각화
5.3 tf-idf
5.3.1 tf-idf 계산
5.3.2 연습문제
5.4 감성 분석
5.4.1 사전 소개
5.4.2 감성 분석 점수