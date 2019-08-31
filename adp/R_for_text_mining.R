# 텍스트 분석을 위한 R (박찬엽)
# 참고 : https://mrchypark.github.io/textR/#1


# 1. 데이터 패키지(presidentSpeech) 소개 ------------------------------------------

# if (!requireNamespace("presidentSpeech")) {
#   remotes::install_github("forkonlp/presidentSpeech")
# }
library(presidentSpeech)

# 대통령 조건 확인
get_president()
# [1] "이승만" "윤보선" "박정희" "최규하" "전두환" "노태우" "김영삼" "김대중" "노무현" "이명박"

# 연설 분야 조건 확인
get_field()
# [1] "국정전반"       "정치/사회"      "산업/경제"      "외교/통상"      "국방"          
# [6] "과학기술정보"   "교육"           "문화/체육/관광" "환경"           "기타"    

# 연설 유형 확인
get_event()
# [1] "취임사"      "신년사"      "국회연설"    "기념사"      "만찬사"      "환영사"     
# [7] "치사"        "성명/담화문" "라디오연설"  "기타" 

# 연설 리스트 데이터
library(dplyr)
data(spidx)
glimpse(spidx)
## Observations: 6,681
## Variables: 6
## $ president <chr> "이승만", "이승만", "이승만", "이승만", "이승만", "이승만", "이승만", "이승...
## $ field     <chr> "기타", "국정전반", "정치/사회", "국정전반", "정치/사회", "정치/사회", "국정...
## $ event     <chr> "성명/담화문", "취임사", "성명/담화문", "기타", "성명/담화문", "성명/담화문",...
## $ title     <chr> "학생제군에게", "대통령 취임사(大統領就任辭) ", "민족이 원하는 길을 따를 결심, 국무총...
## $ date      <chr> "1948", "1948.07.24", "1948.07.29", "1948.08.09", "1...
## $ link      <chr> "http://www.pa.go.kr/research/contents/speech/index....

# 데이터를 사용한 필터링 예시
library(dplyr)
spidx %>% 
  filter(president == "윤보선")
## # A tibble: 3 x 6
##   president field  event  title             date   link                    
##   <chr>     <chr>  <chr>  <chr>             <chr>  <chr>                   
## 1 윤보선    국정전반~ 취임사 제2대 윤보선 대통령 취임사~ 1960.~ http://www.pa.go.kr/res~
## 2 윤보선    기타   기타   "윤보선 대통령 부산연설 \"~ 1960.~ http://www.pa.go.kr/res~
## 3 윤보선    기타   기타   "윤보선 대통령 대구연설 \"~ 1960.~ http://www.pa.go.kr/res~

# 연설문 텍스트 가져오기
library(dplyr)
spidx %>% 
  filter(president == "윤보선") %>% 
  top_n(1, wt = desc(date)) %>% 
  pull(link) -> 
  tar
get_speech(tar)
## # A tibble: 1 x 9
##   title   date   president place field event source paragraph content      
##   <chr>   <chr>  <chr>     <chr> <chr> <chr> <chr>      <int> <chr>        
## 1 제2대 윤보~ 1960.~ 윤보선    국내  국정전반~ 취임사~ ""             1 "제2공화국의 초대대통~

# 연습문제 --------------------------------------------------------------------
# 1. presidentSpeech 패키지에서 검색할 수 있는 대통령은 총 몇명인가요?
# 2.윤보선 대통령과 박정희 대통령은 각각 몇 개의 연설문이 있나요?
# 3. nchar() 함수는 글자수를 세주는 함수입니다. 최규하 대통령의 취임사는 총 몇 글자 인가요?


# 2. tidytext -------------------------------------------------------------
library(tidytext)

# token 단위 처리 : unnest_tokens()
# unnest_tokens() 함수는 텍스트 데이터를 token 단위로 풀어내는 동작을 수행
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

# 연설문 중 1개를 가져와서
get_speech(tar) %>%
  # 대통령 컬럼과 연설문 컬럼만 선택한 후
  select(president, content) %>% 
  # 연설문 컬럼을 띄어쓰기 단위로 쪼갠 결과물을 word라는 컬럼으로 출력
  unnest_tokens(
    input = content,
    output = word
  )
## # A tibble: 483 x 2
##    president word          
##    <chr>     <chr>         
##  1 윤보선    제            
##  2 윤보선    2             
##  3 윤보선    공화국의      
##  4 윤보선    초대대통령으로
##  5 윤보선    영예의        
##  6 윤보선    당선을        
##  7 윤보선    얻은          
##  8 윤보선    어제          
##  9 윤보선    나의          
## 10 윤보선    감격은        

# 연습문제 --------------------------------------------------------------------
# 1. 김영삼 대통령의 첫 국무회의 연설문을 띄어쓰기 단위로 자르면 총 몇 단어인가요?
spidx %>% 
  filter(president == "김영삼") %>% 
  top_n(1, wt = desc(date)) %>% 
  pull(link) %>%
  get_speech() %>%
  # 대통령 컬럼과 연설문 컬럼만 선택한 후
  select(president, content) %>% 
  # 연설문 컬럼을 띄어쓰기 단위로 쪼갠 결과물을 word라는 컬럼으로 출력
  unnest_tokens(
    input = content,
    output = word
  ) %>%
  nrow()
# 정답 : 1125단어?

# 3. 형태소 분석 ---------------------------------------------------------------
# RmecabKo::install_mecab("c:/mecab")
# install.packages("RcppMeCab")
# if (!requireNamespace("RmecabKo")) {
#   install.packages("RmecabKo", repos = "https://cloud.r-project.org")
# }
library(RcppMeCab)
RcppMeCab::pos(iconv("롯데마트가 판매하고 있는 흑마늘 양념 치킨이 논란이 되고 있다.", to = "utf8"))
## $`롯데마트가 판매하고 있는 흑마늘 양념 치킨이 논란이 되고 있다.`
##  [1] "롯데마트/NNP" "가/JKS"       "판매/NNG"     "하/XSV"      
##  [5] "고/EC"        "있/VX"        "는/ETM"       "흑/NNG"      
##  [9] "마늘/NNG"     "양념/NNG"     "치킨/NNG"     "이/JKS"      
## [13] "논란/NNG"     "이/JKS"       "되/VV"        "고/EC"       
## [17] "있/VX"        "다/EF"        "./SF"

library(KoNLP)
SimplePos09("롯데마트가 판매하고 있는 흑마늘 양념 치킨이 논란이 되고 있다.")
## $롯데마트가
## [1] "롯데마트가/N"
## 
## $판매하고
## [1] "판매/N+하고/J"
## 
## $있는
## [1] "있/P+는/E"
## 
## $흑마늘
## [1] "흑마늘/N"
## 
## $양념
## [1] "양념/N"
## 
## $치킨이
## [1] "치킨/N+이/J"
## 
## $논란이
## [1] "논란/N+이/J"
## 
## $되고
## [1] "되/P+고/E"
## 
## $있다

# 생각해보기 -------------------------------------------------------------------
# 1. 노태우 대통령의 취임사를 RcppMeCab 패키지의 pos() 함수로 형태소 분석한 결과를 출력하세요.
library(stringr)
spidx %>%
  filter(president == "노태우") %>%
  str_detect(title, "취임사")
# 2. 김대중 대통령의 취임사를 KoNLP 패키지의 SimplePos09() 함수로 형태소 분석한 결과를 출력하세요.


# 3.1 형태소로 token화 ---------------------------------------------------------

# 데이터 준비
library(tidytext)
library(dplyr)
library(stringr)
library(presidentSpeech)
spidx %>% 
  filter(president == "이명박") %>% 
  filter(str_detect(title, "취임사")) %>% 
  pull(link) -> 
  tar

# 연설문 가져와서 word(띄어쓰기) 단위로 나누기
# 연설문 중 1개를 가져와서
# get_speech(tar, paragraph = T) %>%
#   # 문단 컬럼과 연설문 컬럼만 선택한 후
#   select(paragraph, content) %>% 
#   # 연설문 컬럼을 word 단위로 쪼갠 결과물을 word라는 컬럼으로 출력
#   unnest_tokens(
#     input = content,
#     output = word
#   )
## # A tibble: 2,019 x 2
##    paragraph word    
##        <int> <chr>   
##  1         1 존경하는
##  2         1 국민    
##  3         1 여러분  
##  4         2 700     
##  5         2 만      
##  6         2 해외동포
##  7         2 여러분  
##  8         3 이      
##  9         3 자리에  
## 10         3 참석하신
## # ... with 2,009 more rows

# 형태소 분석기와 함께 사용하기
# unnest_tokens(
#   tbl = 텍스트 데이터,      
#   input = 목표 텍스트 열,   
#   output = 결과열의 이름,   
#   token = "words",       <- 여기에 형태소 분석 함수를 적용
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
## # A tibble: 2,216 x 3
##    paragraph pos              pos_order
##        <int> <chr>                <int>
##  1         1 존경/n+하/x+는/e         1
##  2         1 국민/n                   2
##  3         1 여러분/n                 3
##  4         1 !/s                      4
##  5         2 700/n+만/j               5
##  6         2 해외동포/n               6
##  7         2 여러분/n                 7
##  8         2 !/s                      8
##  9         3 이/m                     9
## 10         3 자리/n+에/j             10
## # ... with 2,206 more rows

# 3.2 불용어 제거 --------------------------------------------------------------

# 필요한 형태소 정보만 선택
library(stringr)
pos_res %>%
  # 우선 `filter()` 와 `str_detect()` 함수를 활용하여 명사(n)만 추출
  filter(str_detect(pos, "/n")) %>% 
  # 형태소 정보를 제거
  mutate(pos_done = str_remove(pos, "/.*$")) -> 
  n_done
n_done
## # A tibble: 1,325 x 4
##    paragraph pos                         pos_order pos_done                
##        <int> <chr>                           <int> <chr>                   
##  1         1 존경/n+하/x+는/e                    1 존경                    
##  2         1 국민/n                              2 국민                    
##  3         1 여러분/n                            3 여러분                  
##  4         2 700/n+만/j                          5 700                     
##  5         2 해외동포/n                          6 해외동포                
##  6         2 여러분/n                            7 여러분                  
##  7         3 자리/n+에/j                        10 자리                    
##  8         3 참석/n+하/x+시ㄴ/e                 11 참석         

# 명사, 형용사, 동사 가져오기
# 명사는 n, 동사/형용사는 p로 표시. 형태소 분석 후 한 글자는 전후 맥락 없이 의미를 파악하기 어렵기 때문에 제거
pos_res %>%
  filter(str_detect(pos, "/p")) %>% 
  mutate(
    pos_done = 
      str_replace_all(pos, "/.*$", "다")
  ) ->
  p_done
bind_rows(n_done, p_done) %>% 
  arrange(pos_order) %>% 
  filter(nchar(pos_done) > 1) %>% 
  select(paragraph, pos_done) ->
  pos_done
pos_done
## # A tibble: 1,647 x 2
##    paragraph pos_done                   
##        <int> <chr>                      
##  1         1 존경                       
##  2         1 국민                       
##  3         1 여러분                     
##  4         2 700                        
##  5         2 해외동포                   
##  6         2 여러분                     
##  7         3 자리                       
##  8         3 참석                       
##  9         3 노무현ㆍ김대중ㆍ김영삼ㆍ전두환
## 10         3 대통령                     
## # ... with 1,637 more rows

# 함께 사용한 함수 설명

# str_detect() : 글자 데이터 내에 찾고자 하는 글자가 있는 지를 T/F로 알려줌
# str_detect(
#   string = 글자 데이터, 
#   pattern = 찾고자 하는 글자,  
#   negate = FALSE # 조건에 맞는 경우 or 그 반대의 결과를 받을 것을 지정            
# )

# str_replace_all() : 찾고자 하는 글자를 원하는 글자로 바꿔줌
# str_replace_all(
#   string = 글자 데이터,
#   pattern = 찾고자 하는 글자,
#   replacement = 찾은 글자가 바뀌게 될 글자
# )

# str_length() : 글자 데이터를 받아서 글자수를 알려줌
# str_count(
#   string = 세고자 하는 글자
# )

# 3.3 정규 표현식 --------------------------------------------------------------

# 글자를 다루는데 유용한 기능을 제공
# ^ : 이걸로 시작함
# $ : 이걸로 끝남
# . : 임의의 글자 하나
# ? : 앞에 있는 문자가 없거나 하나
# + : 앞에 있는 문자가 하나 이상
# * : 앞에 있는 문자가 없거나 하나 이상
# 참고 : https://mrchypark.github.io/dabrp_classnote3/class4

# 연습문제 --------------------------------------------------------------------
# 1. 아래 코드로 이명박 대통령 연설문 중 10개 를 가져오세요. stringr 패키지의 함수를 연습합니다.
# 2. str_detect() 함수를 사용해서 제목(title 컬럼)에 나눔 글자가 포함되어 있는 연설문을 찾으세요.
# 3. str_replace_all() 함수를 사용해서 제목(title 컬럼)에 인터넷 연설 글자를 없애보세요. 아래 문제부터는 다시 tar를 사용해주세요. 명사만 가져오는 과정을 연습합니다.
# 4. mutate() 함수를 사용해서 연설문id를 1~10까지 id 컬럼으로 추가하세요.
# 5. id와 content 컬럼만 선택하세요.
# 6. 형태소 분석을 하여 결과를 pos 컬럼으로 추가하세요.
# 7. str_detect() 함수를 사용해서 명사만 남겨보세요.
# 8. str_replace_all() 함수를 사용해서 POS 정보를 지우고 한글만 남기세요.

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

# 3.4  텍스트 마이닝 지표 ---------------------------------------------------------
# 1. 단어 출현 빈도 : 단순히 단어가 나타난 횟수를 세서 확인
# 2. 동시 출현 빈도 : 기준 단어와 함께 나타난 단어들과 그 횟수를 세서 확인
# 3. tf-idf : 전체 문서에서 나타난 횟수와 개별 문서에서 나타난 횟수로 만든 지표
# 4. 감성 분석 : 단어를 점수화한 감성사전을 사용하여 점수를 합산하여 만든 지표

# 3.5 단어 출현 빈도 ------------------------------------------------------------

# 단어 출현 빈도 계산
# count() 함수는 데이터에서 총 몇 번 나왔는지 세어주는 집계함수. group_by()와 함께 사용하여 각 연설문별 출현 횟수 등을 구할 수 있음.
library(dplyr)
pos_done %>% 
  count(pos_done, sort = T) -> 
  wn
wn
## # A tibble: 883 x 2
##    pos_done     n
##    <chr>    <int>
##  1 하다        94
##  2 있다        38
##  3 국민        30
##  4 우리        26
##  5 여러분      18
##  6 대한민국    17
##  7 되다        16
##  8 만들다      13

# 사용예 : 워드클라우드
# count() 함수로 단어와 그 빈도 테이블을 만들었다면, {wordcloud} 패키지를 사용해서 워드클라우드를 만들 수 있음 {showtext} 패키지를 출력 결과물의 폰트를 설정하기 위한 패키지로 Google Fonts에서 폰트 데이터를 받아와서 출력물에 사용할 수 있음.
# https://fonts.google.com/
library(wordcloud)
library(showtext)
font_add_google("Noto Sans", "notosans")
showtext_auto()
wn %>% 
  with(wordcloud(pos_done, n, family = "notosans"))

# 빈도에 따른 색 입히기
# https://github.com/EmilHvitfeldt/r-color-palettes 에 R에서 사용할 수 있는 색 테마 패키지들을 소개하고 있음.
# install.packages("Redmonder")
library(Redmonder)
pal = redmonder.pal(6, "sPBIRdPu")
wn %>% 
  with(wordcloud(pos_done, 
                 n, 
                 family = "notosans",
                 colors = pal))

# 연습문제 --------------------------------------------------------------------
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
## # A tibble: 10 x 9
##    title     date  president place field  event source paragraph content   
##    <chr>     <chr> <chr>     <chr> <chr>  <chr> <chr>      <int> <chr>     
##  1 제98차 라디오~ ""    이명박    국내  외교/통상~ 라디오연~ ""             1 안녕하십니까, 대~
##  2 제99차 라디오~ ""    이명박    국내  기타   라디오연~ ""             1 안녕하십니까, 대~
##  3 제100차 라디~ ""    이명박    국내  국정전반~ 라디오연~ ""             1 사회자(KBS 아~
##  4 제101차 라디~ ""    이명박    국내  외교/통상~ 라디오연~ ""             1 안녕하십니까, 대~
##  5 제102차 라디~ ""    이명박    국내  과학기술/~ 라디오연~ ""             1 안녕하십니까, 대~
##  6 제103차 라디~ ""    이명박    국내  외교/통상~ 라디오연~ ""             1 안녕하십니까, 대~
##  7 제104차 라디~ ""    이명박    국내  정치/사회~ 라디오연~ ""             1 안녕하십니까, 대~

# 1. tar의 content 컬럼을 pos() 함수로 형태소 분석을 진행해주세요.
# 2. 그 중 명사만 남기고, 형태소 정보는 지워주세요. 한글자 명사도 지워주세요.
# 3. count() 함수를 이용해서 단어 출현 빈도를 계산해 주세요.
# 4. wordcloud를 만들어 주세요.
# 5. 다른 색 조합으로 시도해 주세요.
# 6. group_by()를 활용하여 각 연설문 별로 단어 출현 빈도를 계산해주세요.
# 7. 각 연설문에서 "우리"가 몇 번 사용되었는지 확인해주세요.

# 3.6 동시 출현 빈도 ------------------------------------------------------------

# 동시 출현 빈도 계산
# pairwise_count() : 그룹 단위 내에서 단어가 동시에 출현한 횟수를 세어주는 함수. 보통 문장 단위를 그룹으로 처리
# install.packages("widyr",dependencies = T)
library(widyr)
library(KoNLP)
tar %>% 
  unnest_tokens(sent, content, token = "sentences") %>% 
  mutate(id = as.numeric(1:n())) %>% 
  unnest_tokens(pos, sent, token = SimplePos09) %>% 
  select(id, pos) %>% 
  filter(str_detect(pos, "/n|/v(v|a)")) %>% 
  mutate(pos = str_remove_all(pos, "/.*$")) %>% 
  filter(nchar(pos) > 1) %>% 
  pairwise_count(pos, id, sort = T, upper = F) -> 
  pw
## # A tibble: 15,967 x 3
##    item1  item2      n
##    <chr>  <chr>  <dbl>
##  1 국민   여러분    50
##  2 국민   사랑      22
##  3 여러분 사랑      22
##  4 국민   우리      19
##  5 생각   우리      14
##  6 우리   세계      14
##  7 우리   나라      12
##  8 안녕   대통령    10
##  9 우리   사실      10
## 10 우리   정부      10
## # ... with 15,957 more rows

# pairwise_count(
#   tbl = 대상 데이터,
#   item = 갯수를 새어야 할 컬럼,
#   feature = 함께 출현했다고 판단할 단위 그룹,
#   sort = 출현 횟수 단위로 정렬할지
# )

# 기준 단어로 데이터 탐색
# filter() : 기준 단어를 조회하면 함께 자주 나오는 단어와 그 빈도를 확인할 수 있음
pw %>%
  filter(item1 == "우리")
## # A tibble: 648 x 3
##    item1 item2        n
##    <chr> <chr>    <dbl>
##  1 우리  세계        14
##  2 우리  나라        12
##  3 우리  사실        10
##  4 우리  정부        10
##  5 우리  이번         8
##  6 우리  대한민국     8
##  7 우리  경제         8
##  8 우리  개발         7
##  9 우리  모두         6
## 10 우리  국가         6
## # ... with 638 more rows
library(forcats)
library(ggplot2)
# bar plot
pw %>%
  filter(item1 %in% c("우리")) %>% 
  top_n(15) %>% 
  mutate(item2 = fct_reorder(item2, n, .desc = TRUE)) %>% 
  ggplot(aes(x = item2, y = n, fill = item1)) + geom_bar(stat = "identity")

# 연습문제 --------------------------------------------------------------------
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
## # A tibble: 10 x 9
##    title     date  president place field  event source paragraph content   
##    <chr>     <chr> <chr>     <chr> <chr>  <chr> <chr>      <int> <chr>     
##  1 제98차 라디오~ ""    이명박    국내  외교/통상~ 라디오연~ ""             1 안녕하십니까, 대~
##  2 제99차 라디오~ ""    이명박    국내  기타   라디오연~ ""             1 안녕하십니까, 대~
##  3 제100차 라디~ ""    이명박    국내  국정전반~ 라디오연~ ""             1 사회자(KBS 아~
##  4 제101차 라디~ ""    이명박    국내  외교/통상~ 라디오연~ ""             1 안녕하십니까, 대~
##  5 제102차 라디~ ""    이명박    국내  과학기술/~ 라디오연~ ""             1 안녕하십니까, 대~
##  6 제103차 라디~ ""    이명박    국내  외교/통상~ 라디오연~ ""             1 안녕하십니까, 대~
##  7 제104차 라디~ ""    이명박    국내  정치/사회~ 라디오연~ ""             1 안녕하십니까, 대~

# 1. 앞장의 코드를 이용해서 tar 데이터를 만들어주세요.
# 2. tar의 content 컬럼을 문장 단위로 나누어 주세요.
# 3. 새롭게 문장별 id를 id 컬럼으로 추가해주세요.
# 4. 문장별 id를 유지한 채로 pos() 함수를 사용하여 형태소 분석을 진행해 주세요.
# 5. 명사(/n), 동사(/vv), 형용사(/va)인 형태소만 가져와 주세요.
# 6. 형태소 정보는 제거하지 말고 그대로 두세요.
# 7. 동시 출현 빈도 테이블을 만들어 주세요. (컬럼이 item1, item2, n으로 구성됩니다.)
# 8. 우리/np와 함께 출현한 단어들과 그 빈도를 확인하세요.
# 9. 명사는 형태소 정보를 제거하고, 형용사와 동사는 형태소 정보를 제거한후 뒤에 다를 붙여주세요.
# 10. 한 글자는 제거해 주세요.
# 11. 동시 출현 빈도 테이블을 만들어 주세요. (컬럼이 item1, item2, n으로 구성됩니다.)
# 12. 사랑과 함께 출현한 단어들과 그 빈도를 확인하세요.

# 예시 답안
# tar %>% 
#   mutate(speech_id = 1:nrow(.)) %>% 
#   unnest_tokens(senten, content, 
#                 token="sentences") %>% 
#   select(speech_id, senten) %>% 
#   mutate(sentence_id = 1:nrow(.)) %>% 
#   unnest_tokens(morph, senten,
#                 token = pos) %>% 
#   filter(str_detect(morph, "/n|/v(v|a)")) %>% 
#   mutate(
#     morph = if_else(
#       str_detect(morph, "/n"),
#       str_replace_all(morph, "/.*$", ""),
#       str_replace_all(morph, "/.*$", "다")
#     )
#   ) %>% 
#   filter(str_length(morph) > 1) %>% 
#   pairwise_count(morph, sentence_id, sort=T, upper=F) %>% 
#   filter(item1 == "사랑")

# 사용예 : 네트워크 시각화
library(igraph)
pw %>% 
  filter(n > 5) %>%
  graph_from_data_frame() ->
  pw_graph
pw_graph
## IGRAPH 2063ffb DN-- 34 35 -- 
## + attr: name (v/c), n (e/n)
## + edges from 2063ffb (vertex names):
##  [1] 국민          ->여러분   국민          ->사랑    
##  [3] 여러분        ->사랑     국민          ->우리    
##  [5] 생각          ->우리     우리          ->세계    
##  [7] 우리          ->나라     안녕          ->대통령  
##  [9] 우리          ->사실     우리          ->정부    
## [11] 때문          ->우리     사회          ->우리    
## [13] 국민          ->생각     미래          ->우리    
## [15] 우리          ->이번     국민          ->모두    
## + ... omitted several edges

# 네트워크 데이터는 node, edge로 구성됨
library(ggraph)
set.seed(2018)
a <- grid::arrow(type = "closed", length = unit(.1, "inches"))
ggraph(pw_graph) +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# 3.7 tf-idf 계산 -----------------------------------------------------------
# bind_tf_idf() 함수가 tf, idf, tf-idf 점수 모두를 제공하며 문서 단위의 정의가 매우 중요함. 보통 각 연설문, 개별 뉴스 본문 등을 하나의 문서로 정의함. tf-idf 가 높을 수록 각 문서에서 특별한 의미를 지니는 것으로 판단할 수 있음.
tar %>% 
  mutate(id = as.numeric(1:n())) %>% 
  # unnest_tokens(pos, content, token = pos) %>% 
  unnest_tokens(pos, content, token = pos) %>%
  select(id, pos) %>% 
  filter(str_detect(pos, "/n|/v(v|a)")) %>% 
  mutate(pos = str_remove_all(pos, "/.*$")) %>% 
  filter(nchar(pos) > 1) %>% 
  group_by(id) %>% 
  count(pos) -> 
  tfidf_tar

tfidf_tar %>% 
  bind_tf_idf(pos, id, n) %>% 
  arrange(desc(tf_idf))
## # A tibble: 0 x 6
## # Groups:   id [0]
## # ... with 6 variables: id <dbl>, pos <chr>, n <int>, tf <dbl>, idf <dbl>,
## #   tf_idf <dbl>

# 연습문제 --------------------------------------------------------------------
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
## # A tibble: 10 x 9
##    title     date  president place field  event source paragraph content   
##    <chr>     <chr> <chr>     <chr> <chr>  <chr> <chr>      <int> <chr>     
##  1 제98차 라디오~ ""    이명박    국내  외교/통상~ 라디오연~ ""             1 안녕하십니까, 대~
##  2 제99차 라디오~ ""    이명박    국내  기타   라디오연~ ""             1 안녕하십니까, 대~
##  3 제100차 라디~ ""    이명박    국내  국정전반~ 라디오연~ ""             1 사회자(KBS 아~
##  4 제101차 라디~ ""    이명박    국내  외교/통상~ 라디오연~ ""             1 안녕하십니까, 대~
##  5 제102차 라디~ ""    이명박    국내  과학기술/~ 라디오연~ ""             1 안녕하십니까, 대~
##  6 제103차 라디~ ""    이명박    국내  외교/통상~ 라디오연~ ""             1 안녕하십니까, 대~
##  7 제104차 라디~ ""    이명박    국내  정치/사회~ 라디오연~ ""             1 안녕하십니까, 대~

# 1. 새롭게 연설문별 id를 id 컬럼으로 추가해주세요.
# 2. 문장별 id를 유지한 채로 pos() 함수를 사용하여 형태소 분석을 진행해 주세요.
# 3. 명사(/n), 동사(/vv), 형용사(/va)인 형태소만 가져와 주세요.
# 4. 명사는 형태소 정보를 제거하고, 형용사와 동사는 형태소 정보를 제거한후 뒤에 다를 붙여주세요.
# 5.한 글자는 제거해 주세요.
# 6. 연설문 별로 형태소 단위 빈도를 계산해 주세요.
# 7. bind_tf_idf() 함수를 사용해서 tf, idf, tf-idf 를 계산해주세요.
# 8. 각 연설문 별로 tf-idf 점수가 가장 높은 단어를 확인하세요.
# 9. 각 연설문 별로 tf-idf 점수가 가장 높은 3개 단어씩을 확인하세요.

# 3.8 감성분석 ----------------------------------------------------------------
# 감성 분석은 각 단어의 감성 사전을 구축하여 점수를 주는 방식
# 한글의 특성상, 형태소이며 ngram에 점수를 부여하는 것이 가장 효과적일 것
# 단순한 형태로는 unigram의 형태소에 점수나 종류를 부여하는 것
# 개별 단어의 점수를 부여한 뒤 문장 단위로 합산하여 계산
# 합산으로 0에 가까운 값이 나올 수 도 있으므로 점수를 부여받은 단어의 갯수등 도 고려 필요
# 안정적으로 기구축된 한글 사전을 찾기 어려움

# 사전소개
# KnuSentiLex는 군산대 Data Intelligence Lab에서 기존 사전들을 참조, 활용하여 18년 구축한 감성 사전. 구조가 단순하고 이모티콘 등을 추가한 점이 장점인 반면, 형태소 형식이 아니라 점수의 신뢰도가 낮은 편임.
# KOSAC은 서울대에서 13년에 구축한 감성사전으로 구조가 복잡하고 점수를 내기 어렵지만 12년에 구축한 감성 스키마를 따르고 있어 다양한 감성 정보를 얻을 수 있음.
# 본 예시에는 구조가 단순한 KnuSentiLex을 사용

# remotes::install_github("mrchypark/KnuSentiLexR")
library(KnuSentiLexR)
tar %>% 
  unnest_tokens(sent, content, token = "sentences") %>% 
  filter(nchar(sent) < 20) %>% 
  select(sent) -> 
  senti_tar

# 감성 분석 점수
# senti_score() 함수는 문장을 unigram 부터 3-gram 까지 작성한 후, 감성 사전에 점수를 합산하여 문장 점수를 계산
# senti_magnitude() 함수는 몇개의 ngram이 점수화되었는지를 계산
# dic 객체가 word, polarity 컬럼을 가지고 있는 감성 사전임
senti_tar %>% 
  mutate(score = senti_score(sent),
         magni = senti_magnitude(sent)) %>% 
  filter(score != 0)
## # A tibble: 38 x 3
##    sent                              score magni
##    <chr>                             <dbl> <dbl>
##  1 먼저 함께 보시죠.                     1     1
##  2 자랑 좀 해 보세요.                   -2     1
##  3 사회자 눈물이 그렁그렁하네요.        -1     1
##  4 이상하게.                            -1     1
##  5 이분은 ‘한번 해 보자.’               -2     1
##  6 대통령 그렇게 해 주세요.             -2     1
##  7 “난 부모 잘 만났어요.                 1     1
##  8 갔더니 정말 친절하게 잘해 줬어요.     4     2
##  9 그런데 설명을 다 해 주시더라고요.    -2     1
## 10 그거 대단한 거죠.                     2     1
## # ... with 28 more rows

