# 출처 - ADP 공식 수험서 > 과목4 데이터 분석 > 제2장 통계분석 p395~p396

# [문제] 다음은 MASS 패키지의 hills 데이터다. step 함수를 이용해 전진선택법을 적용하시오. 이때 time은 종속변수다.

library(MASS)
data(hills)
head(hills)
# dist climb   time
# Greenmantle   2.5   650 16.083
# Carnethy      6.0  2500 48.350
# Craig Dunain  6.0   900 33.650
# Ben Rha       7.5   800 45.600
# Ben Lomond    8.0  3070 62.267
# Goatfell      8.0  2866 73.217

step(lm(time~1, hills), scope=list(lower=~1,upper=~dist+climb), direction="forward")
# Start:  AIC=274.88
# time ~ 1
# 
# Df Sum of Sq   RSS    AIC
# + dist   1     71997 13142 211.49
# + climb  1     55205 29934 240.30
# <none>               85138 274.88
# 
# Step:  AIC=211.49
# time ~ dist
# 
# Df Sum of Sq     RSS    AIC
# + climb  1    6249.7  6891.9 190.90
# <none>               13141.6 211.49
# 
# Step:  AIC=190.9
# time ~ dist + climb
# 
# 
# Call:
#   lm(formula = time ~ dist + climb, data = hills)
# 
# Coefficients:
#   (Intercept)         dist        climb  
# -8.99204      6.21796      0.01105  