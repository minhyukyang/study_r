# 출처 - ADP 공식 수험서 > 과목4 데이터 분석 > 제2장 통계분석 p388~p395

# [문제] 다음과 같은 데이터가 있다. Y를 반응변수로 하고, X1, X2, X3, X4를 설명변수로 하는 선형회귀모형을 고려하고, 후진제거법을 이용하여 변수를 선택하시오.

# 데이터 생성
x1 <- c(7,1,11,11,7,11,3,1,2,21,1,11,10)
x2 <- c(26,29,56,31,52,55,71,31,54,47,40,66,68)
x3 <- c(6,15,8,8,6,9,17,22,18,4,23,9,8)
x4 <- c(60,52,20,47,33,22,6,44,22,26,34,12,12)
y <- c(78.5,74.3,104.3,87.6,95.9,109.2,102.7,72.5,93.1,115.9,83.8,113.3,109.4)

# 데이터프레임 형태로 변형
df <- data.frame(x1,x2,x3,x4,y)
head(df)
# x1 x2 x3 x4     y
# 1  7 26  6 60  78.5
# 2  1 29 15 52  74.3
# 3 11 56  8 20 104.3
# 4 11 31  8 47  87.6
# 5  7 52  6 33  95.9
# 6 11 55  9 22 109.2

a <- lm(y~x1+x2+x3+x4, data=df)
a
# Call:
#   lm(formula = y ~ x1 + x2 + x3 + x4, data = df)
# 
# Coefficients:
#   (Intercept)           x1           x2           x3           x4  
# 62.4054       1.5511       0.5102       0.1019      -0.1441

summary(a)
# Call:
#   lm(formula = y ~ x1 + x2 + x3 + x4, data = df)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max
# -3.1750 -1.6709  0.2508  1.3783  3.9254
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  62.4054    70.0710   0.891   0.3991
# x1            1.5511     0.7448   2.083   0.0708 .
# x2            0.5102     0.7238   0.705   0.5009
# x3            0.1019     0.7547   0.135   0.8959
# x4           -0.1441     0.7091  -0.203   0.8441
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.446 on 8 degrees of freedom
# Multiple R-squared:  0.9824,	Adjusted R-squared:  0.9736
# F-statistic: 111.5 on 4 and 8 DF,  p-value: 4.756e-07

b <- lm(y~x1+x2+x4,data=df)
summary(b)
# Call:
#   lm(formula = y ~ x1 + x2 + x4, data = df)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.0919 -1.8016  0.2562  1.2818  3.8982 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  71.6483    14.1424   5.066 0.000675 ***
#   x1            1.4519     0.1170  12.410 5.78e-07 ***
#   x2            0.4161     0.1856   2.242 0.051687 .  
# x4           -0.2365     0.1733  -1.365 0.205395    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.309 on 9 degrees of freedom
# Multiple R-squared:  0.9823,	Adjusted R-squared:  0.9764 
# F-statistic: 166.8 on 3 and 9 DF,  p-value: 3.323e-08

c <- lm(y~x1+x2,data=df)
summary(c)
# Call:
#   lm(formula = y ~ x1 + x2, data = df)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -2.893 -1.574 -1.302  1.363  4.048 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 52.57735    2.28617   23.00 5.46e-10 ***
#   x1           1.46831    0.12130   12.11 2.69e-07 ***
#   x2           0.66225    0.04585   14.44 5.03e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.406 on 10 degrees of freedom
# Multiple R-squared:  0.9787,	Adjusted R-squared:  0.9744 
# F-statistic: 229.5 on 2 and 10 DF,  p-value: 4.407e-09

# 예제1 : 전진선택법
step(lm(y~1,df), scope=list(lower=~1,upper=~x1+x2+x3+x4), direction="forward")
# Start:  AIC=71.44
# y ~ 1
# 
# Df Sum of Sq     RSS    AIC
# + x4    1   1831.90  883.87 58.852
# + x2    1   1809.43  906.34 59.178
# + x1    1   1450.08 1265.69 63.519
# + x3    1    776.36 1939.40 69.067
# <none>              2715.76 71.444
# 
# Step:  AIC=58.85
# y ~ x4
# 
# Df Sum of Sq    RSS    AIC
# + x1    1    809.10  74.76 28.742
# + x3    1    708.13 175.74 39.853
# <none>              883.87 58.852
# + x2    1     14.99 868.88 60.629
# 
# Step:  AIC=28.74
# y ~ x4 + x1
# 
# Df Sum of Sq    RSS    AIC
# + x2    1    26.789 47.973 24.974
# + x3    1    23.926 50.836 25.728
# <none>              74.762 28.742
# 
# Step:  AIC=24.97
# y ~ x4 + x1 + x2
# 
# Df Sum of Sq    RSS    AIC
# <none>              47.973 24.974
# + x3    1   0.10909 47.864 26.944
# 
# Call:
#   lm(formula = y ~ x4 + x1 + x2, data = df)
# 
# Coefficients:
#   (Intercept)           x4           x1           x2  
# 71.6483      -0.2365       1.4519       0.4161  

# 예제2 : 단계적 변수선택법 
step(lm(y~1,df), scope=list(lower=~1,upper=~x1+x2+x3+x4), direction="both")
# Start:  AIC=71.44
# y ~ 1
# 
# Df Sum of Sq     RSS    AIC
# + x4    1   1831.90  883.87 58.852
# + x2    1   1809.43  906.34 59.178
# + x1    1   1450.08 1265.69 63.519
# + x3    1    776.36 1939.40 69.067
# <none>              2715.76 71.444
# 
# Step:  AIC=58.85
# y ~ x4
# 
# Df Sum of Sq     RSS    AIC
# + x1    1    809.10   74.76 28.742
# + x3    1    708.13  175.74 39.853
# <none>               883.87 58.852
# + x2    1     14.99  868.88 60.629
# - x4    1   1831.90 2715.76 71.444
# 
# Step:  AIC=28.74
# y ~ x4 + x1
# 
# Df Sum of Sq     RSS    AIC
# + x2    1     26.79   47.97 24.974
# + x3    1     23.93   50.84 25.728
# <none>                74.76 28.742
# - x1    1    809.10  883.87 58.852
# - x4    1   1190.92 1265.69 63.519
# 
# Step:  AIC=24.97
# y ~ x4 + x1 + x2
# 
# Df Sum of Sq    RSS    AIC
# <none>               47.97 24.974
# - x4    1      9.93  57.90 25.420
# + x3    1      0.11  47.86 26.944
# - x2    1     26.79  74.76 28.742
# - x1    1    820.91 868.88 60.629
# 
# Call:
#   lm(formula = y ~ x4 + x1 + x2, data = df)
# 
# Coefficients:
#   (Intercept)           x4           x1           x2  
# 71.6483      -0.2365       1.4519       0.4161  
