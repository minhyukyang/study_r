# install.packages("esquisse")
# install.packages("httpuv")
# install.packages("maps")

#####
# 1. esquisse 패키지 설치
# 2. 위도, 경도가 포함된 데이터 로드
# 3. `esquisse()` 실행
# 4. data.frame 선택 후 다음 단계 선택
# 5. x = log, y = lat, color/facet = company 드래그
# 6. 최종 선택된 함수는 우측 하단의 버튼으로 editor에 code 삽입 가능

library(esquisse)

wifi_data <- read.csv("u:/wifi.csv")

esquisser()


#####
# 7.시각화 결과 지도로 확인 
library(ggplot2)

ggplot(wifi) +
  aes(x = lon, y = lat, colour = company) +
  geom_point(size = 1L) +
  scale_color_hue() +
  theme_minimal()


#####
# 8.아래 링크를 통해 추가 작업이 필요합니다.

# 1) Google 개발자 등록 및 API Key 만들기
# Google API는 무료가 아니며 월기준 40,000 호출횟수까지는 무료입니다. (초과시 건당 0.005달러 청구)
# 위 조건에 따라 API발급 시 신용카드 등록이 필수 입니다.
# 참고 : Google API 가입 및 시각화 예제(https://mrkevinna.github.io/R-%EC%8B%9C%EA%B0%81%ED%99%94-3/)

# 2) 활용 사례
# 공간통계를 위한 데이터 사이언스 : https://statkclee.github.io/spatial/geo-info-lonlat.html  
# leaflet 및 ggmap 을 이용한 지도 구현 사례 : http://lumiamitie.github.io/r_tutorial/blog_link/leaflet_in_r.html
# ggmap으로 지도에 표시하기 : https://aworklab.tistory.com/25

# 3) 첨언
# 오픈으로 공개된 맵을 이용할 경우 퀄리티가 낮아 원하는 결과를 얻기 힘듭니다.
# install.packages("OpenStreetMap")
library(OpenStreetMap)
map <- openmap(c(43.46886761482925,119.94873046875),
               c(33.22949814144951,133.9892578125),
               minNumTiles=4)
autoplot(map)

# 대신 아래 링크에서 제공하는 맵데이터를 받아서 적용 가능한지 테스트가 필요해 보입니다.
# https://openmaptiles.com/downloads/dataset/osm/asia/south-korea/#5.51/35.567/128.329
