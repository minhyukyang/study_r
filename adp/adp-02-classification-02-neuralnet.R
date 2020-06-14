# 2. 신경망 모형

data(iris)

# 1. nnet -----------------------------------------------------------------

library(nnet)

nn.iris <- nnet(Species~., data=iris, size=2, rang=0.1, decay=5e-4, maxit=200)
# # weights:  19
# initial  value 164.719058 
# iter  10 value 71.194376
# iter  20 value 58.150714
# iter  30 value 15.112105
# iter  40 value 8.479792
# iter  50 value 8.341673
# iter  60 value 8.289768
# iter  70 value 8.280405
# iter  80 value 8.263311
# iter  90 value 8.255909
# iter 100 value 8.255441
# iter 110 value 8.255207
# iter 120 value 8.254237
# iter 130 value 7.974823
# iter 140 value 5.401102
# iter 150 value 5.149102
# iter 160 value 5.076455
# iter 170 value 5.029285
# iter 180 value 5.024859
# iter 190 value 5.021491
# iter 200 value 5.020654
# final  value 5.020654 
# stopped after 200 iterations

summary(nn.iris)
# a 4-2-3 network with 19 weights
# options were - softmax modelling  decay=5e-04
# b->h1 i1->h1 i2->h1 i3->h1 i4->h1 
# -3.12  -0.08  -0.39   1.42  -0.84 
# b->h2 i1->h2 i2->h2 i3->h2 i4->h2 
# 11.20   2.19   8.08   0.25 -27.53 
# b->o1 h1->o1 h2->o1 
# 7.49 -37.27   7.73 
# b->o2 h1->o2 h2->o2 
# 0.25  -0.44   7.48 
# b->o3 h1->o3 h2->o3 
# -7.74  37.71 -15.21

table(iris$Species, predict(nn.iris, iris, type="class"))
#              setosa versicolor virginica
# setosa         50          0         0
# versicolor      0         50         0
# virginica       0          1        49


# 2. neuralnet ----------------------------------------------------------

install.packages("neuralnet")
library(neuralnet)

data(infert)
str(infert)
#'data.frame':	248 obs. of  8 variables:
# $ education     : Factor w/ 3 levels "0-5yrs","6-11yrs",..: 1 1 1 1 2 2 2 2 2 2 ...
# $ age           : num  26 42 39 34 35 36 23 32 21 28 ...
# $ parity        : num  6 1 6 4 3 4 1 2 1 2 ...
# $ induced       : num  1 1 2 2 1 2 0 0 0 0 ...
# $ case          : num  1 1 1 1 1 1 1 1 1 1 ...
# $ spontaneous   : num  2 0 0 0 1 1 0 0 1 0 ...
# $ stratum       : int  1 2 3 4 5 6 7 8 9 10 ...
# $ pooled.stratum: num  3 1 4 2 32 36 6 22 5 19 ...

net.infert <- neuralnet(case ~ age+parity+induced+spontaneous, data=infert, hidden=2, err.fct="ce", linear.output=FALSE, likelihood=TRUE)
net.infert

plot(net.infert)

net.infert$result.matrix
#                                 [,1]
# error                    1.252136e+02
# reached.threshold        9.643059e-03
# steps                    4.294000e+03
# aic                      2.764273e+02
# bic                      3.221019e+02
# Intercept.to.1layhid1    4.044333e+00
# age.to.1layhid1          1.529982e+00
# parity.to.1layhid1       2.530350e+00
# induced.to.1layhid1      2.686474e+00
# spontaneous.to.1layhid1  9.204975e-01
# Intercept.to.1layhid2    5.604180e+00
# age.to.1layhid2         -1.181458e-01
# parity.to.1layhid2       1.778605e+00
# induced.to.1layhid2     -2.213891e+00
# spontaneous.to.1layhid2 -3.392501e+00
# Intercept.to.case        2.412752e+00
# 1layhid1.to.case         8.904553e-01
# 1layhid2.to.case        -5.098002e+00

out <- cbind(net.infert$covariate, net.infert$net.result[[1]])
dimnames(out) <- list(NULL, c("age", "parity", "induced", "spontaneous", "nn-output"))
head(out)
#    age parity induced spontaneous nn-output
# [1,]  26      6       1           2 0.1518897
# [2,]  42      1       1           0 0.6204863
# [3,]  39      6       2           0 0.1429328
# [4,]  34      4       2           0 0.1512583
# [5,]  35      3       1           1 0.3512906
# [6,]  36      4       2           1 0.4898612

# 가중치의 초기값과 적합값은 $startweights와 $weight에 제공됨
# 작은 분산은 선형효과를 제시하며, 큰 분산은 관측치 공간상에서 변화가 심하다는 것을 나타내므로 비선형적인 효과가 있음을 나타냄
head(net.infert$generalized.weights[[1]])
#             [,1]         [,2]        [,3]       [,4]
# [1,] 0.008723565 -0.131327336 0.163467708 0.25049310
# [2,] 0.148978848 -2.242775214 2.791660391 4.27785815
# [3,] 0.000431277 -0.006492582 0.008081543 0.01238392
# [4,] 0.008160011 -0.122843421 0.152907483 0.23431092
# [5,] 0.107233575 -1.614328527 2.009410921 3.07916210
# [6,] 0.135938342 -2.046459267 2.547299097 3.90340610

# `compute()` 함수는 각 뉴런의 출력을 계산함
new.output <- compute(net.infert,
                      covariate = matrix(c(22,1,0,0,
                                           22,1,1,0,
                                           22,1,0,1,
                                           22,1,1,1),
                                         byrow=TRUE, ncol=4))
new.output$net.result
#          [,1]
# [1,] 0.1477323
# [2,] 0.1927483
# [3,] 0.3145164
# [4,] 0.8516333

# 위 결과(new.output$net.result)는 주어진 공변량 조합에 대한 예측 결과로, 사전 낙태의 수에 따라 예측 확률이 증가함을 나타냄 