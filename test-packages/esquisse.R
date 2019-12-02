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

# ggmap으로 지도에 표시하기 : https://aworklab.tistory.com/25
# 공간통계를 위한 데이터 사이언스 : https://statkclee.github.io/spatial/geo-info-lonlat.html  
# leaflet 및 ggmap 을 이용한 지도 구현 : http://lumiamitie.github.io/r_tutorial/blog_link/leaflet_in_r.html
