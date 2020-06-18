# 1. 로지스틱회귀 모형
# 반응변수가 범주형인 경우에 적용되는 회귀분석 모형

set.seed(2020)
data(iris)

library(caret)

# 데이터 변환 (0,1)
a <- subset(iris, Species == "setosa" | Species == "versicolor")
a$Species <- factor(a$Species)
str(a)
# 'data.frame':	100 obs. of  5 variables:
# $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
# $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
# $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
# $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
# $ Species     : Factor w/ 2 levels "setosa","versicolor": 1 1 1 1 1 1 1 1 1 1 ...

# 데이터 분할
idx <- createDataPartition(y=a$Species, p=0.7, list=FALSE)
# ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))

trainData <- a[idx,]
testData <- a[-idx,]

# 모델링
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

iris_logit <- glm(myFormula, data=trainData, family="binomial")
# iris_logit <- glm(myFormula, data=trainData, family="binomial")

summary(iris_logit)
# Call:
# glm(formula = Species ~ Sepal.Length, family = binomial, data = a)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -2.05501  -0.47395  -0.02829   0.39788   2.32915  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -27.831      5.434  -5.122 3.02e-07 ***
#   Sepal.Length    5.140      1.007   5.107 3.28e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 138.629  on 99  degrees of freedom
# Residual deviance:  64.211  on 98  degrees of freedom
# AIC: 68.211
# 
# Number of Fisher Scoring iterations: 6

fitted(iris_logit)[c(1:5, 96:100)] # 적합 결과 확인
#          1          2          3          4          5         96         97         98         99        100 
# 0.16579367 0.06637193 0.02479825 0.01498061 0.10623680 0.81282396 0.81282396 0.98268360 0.16579367 0.81282396

predict(b, newdata=a[c(1, 50, 51, 100),], type="response") # 예측 결과 확인
#        1        50        51       100 
# 0.1657937 0.1062368 0.9997116 0.812824

cdplot(Species ~ Sepal.Length, data=a) # Sepal.Length(연속형 변수)의 변화에 따른 범주형 변수의 조건부 분포
