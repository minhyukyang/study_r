# 3절 군집분석

set.seed(2020)

# 1. Hierarchical Clustering ----------------------------------------------
# 가장 유사한 개체를 묶어 나가는 과정을 반복하여 원하는 개수의 군집을 형성하는 방법
# - 최단연결법 또는 단일연결법 : 두 군집 사이의 거리를 각 군집에서 하나씩 관측값을 뽑았을 때 나타날 수 있는 거리의 최소값으로 측정
# - 최장연결법 또는 완전연결법 : 두 군집 사이의 거리를 각 군집에서 하나씩 관측값을 뽑았을 때 나타날 수 있는 거리의 최대값으로 측정(내부 응집성)
# - 중심연결법 : 두 군집의 중심 간의 거리를 측정
# - 평균연결법 : 모든 항목에 대한 거리 평균을 구하면서 군집화를 하기때문에 불필요한 계산량 발생가능성 있음
# - 와드연결법 : 군집 내 오차제곱합에 기초하여 군집 수행

# using hclust()

idx <- sample(1:nrow(iris), 50)
irisSample <- iris[idx,]
irisSample$Species <- NULL

# fit <- hclust(dist(irisSample), method="ave")
# plot(fit, hang=-1, labels=iris$Species[idx])

fit <- hclust(dist(irisSample, method="euclidean"), method="ave")
par(mfrow=c(1,2))
plot(fit)
plot(fit, hang = -1)
par(mfrow=c(1,1))

groups <- cutree(fit, k=6) # `cutree()는 계층적군집의 결과를 이용하여 tree의 높이(h)나 그룹의 수(k)를 옵션으로 지정하여 원하는 수의 그룹 생성`
groups
# 25 102  89  59  57  19 143  84 148  77   9   1 119  50  39   4  49  56  85  10 131  82  69  73  88  17 113  64  61  30 101 142  20  86   7 
# 1   2   3   2   2   1   2   2   4   2   1   1   5   1   1   1   1   3   3   1   5   3   2   2   2   1   4   2   6   1   4   4   1   2   1 
# 35  68 126  45 123  87   3  83  96   2  67  80  21  60 109 
# 1   3   5   1   5   2   1   3   3   1   3   3   1   3   4

plot(fit)
rect.hclust(fit, k=6, border="red") #  `rect.hclust()`는 그룹수(k)를 이용하여 그룹을 시각화 할 뿐만 아니라, TREE의 높이(h)와 위치(which)를 이용하여 그룹의 전체 또는 일부를 나타낼 수 있음

hca <- hclust(dist(irisSample))
plot(hca)
rect.hclust(hca, k=3, border="red")
rect.hclust(hca, h=50, which=c(1,2), border=3:4)

# using agnes()
library(cluster)

agn1 <- agnes(irisSample, metric="manhattan", stand=TRUE)
agn1
# Call:	 agnes(x = irisSample, metric = "manhattan", stand = TRUE) 
# Agglomerative coefficient:  0.8801991 
# Order of objects:
#   [1] 25  7   1   50  21  9   39  4   30  3   10  35  2   19  49  20  45  17  102 143 84  89  96  85  67  56  82  68  83  80  60  59  77  64 
# [35] 87  57  86  69  88  73  109 148 113 142 126 101 119 131 123 61 
# Height (summary):
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.7726  1.1187  1.5888  2.1458  8.0124 
# 
# Available components:
#   [1] "order"     "height"    "ac"        "merge"     "diss"      "call"      "method"    "order.lab" "data"     

par(mfrow=c(1,2))
plot(agn1)

agn2 <- agnes(daisy(irisSample), diss=TRUE, method="complete")
plot(agn2)

agn3 <- agnes(irisSample, method="flexible", par.meth=0.6)
plot(agn3)
par(mfrow=c(1,1))

# 2. K-menas --------------------------------------------------------------

set.seed(2020)

library(caret)

# dataset : iris
data(iris)

newiris <- iris
newiris$Species <- NULL

kc <- kmeans(newiris, 3)

kc
# K-means clustering with 3 clusters of sizes 50, 38, 62
# 
# Cluster means:
#   Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1     5.006000    3.428000     1.462000    0.246000
# 2     6.850000    3.073684     5.742105    2.071053
# 3     5.901613    2.748387     4.393548    1.433871
# 
# Clustering vector:
#   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
# [70] 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 2 2 2 2 3 2 2 2 2 2 2 3 3 2 2 2 2 3 2 3 2 3 2 2 3 3 2 2 2 2 2 3 2 2 2 2
# [139] 3 2 2 2 3 2 2 2 3 2 2 3
# 
# Within cluster sum of squares by cluster:
#   [1] 15.15100 23.87947 39.82097
# (between_SS / total_SS =  88.4 %)
# 
# Available components:
#   
#   [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"         "iter"         "ifault" 

table(iris$Species, kc$cluster)
#               1  2  3
# setosa     50  0  0
# versicolor  0  2 48
# virginica   0 36 14

plot(newiris[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=2)

kc4 <- kmeans(newiris, 4)
table(iris$Species, kc4$cluster)
#             1  2  3  4
# setosa      0 33  0 17
# versicolor  2  0 48  0
# virginica  36  0 14  0

# dataset : wine
# install.packages("rattle")
data(wine, package="rattle")
head(wine)
df <- scale(wine[-1])

# install.packages("NbClust")
library(NbClust)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])
# 0  1  2  3 10 12 14 15 
# 2  1  4 15  1  1  1  1 

barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters chosen by 26 Criteria")
# 최적의 군집수 선정을 위해 사용되는 지수(총 30개 중 여기서는 26개 지수 계산됨) 가운데 15개의 지수가 3을 최적의 군집수로 투표한 결과

set.seed(2020)
fit.km <- kmeans(df, 3, nstart=25)
fit.km$size
# [1] 65 62 51

fit.km$centers
#      Alcohol      Malic        Ash Alcalinity   Magnesium     Phenols  Flavanoids Nonflavanoids Proanthocyanins      Color        Hue
# 1 -0.9234669 -0.3929331 -0.4931257  0.1701220 -0.49032869 -0.07576891  0.02075402   -0.03343924      0.05810161 -0.8993770  0.4605046
# 2  0.8328826 -0.3029551  0.3636801 -0.6084749  0.57596208  0.88274724  0.97506900   -0.56050853      0.57865427  0.1705823  0.4726504
# 3  0.1644436  0.8690954  0.1863726  0.5228924 -0.07526047 -0.97657548 -1.21182921    0.72402116     -0.77751312  0.9388902 -1.1615122
# Dilution    Proline
# 1  0.2700025 -0.7517257
# 2  0.7770551  1.1220202
# 3 -1.2887761 -0.4059428

plot(df, col=fit.km$cluster) # 군집 출력
points(fit.km$center, col=1:3, pch=16, cex=1.5) # 군집별 중심 출력

aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean) # 군집별 요약값을 측정단위의 척도로 출력
# 1       1 12.25092 1.897385 2.231231   20.06308  92.73846 2.247692  2.0500000     0.3576923        1.624154 2.973077 1.0627077 2.803385
# 2       2 13.67677 1.997903 2.466290   17.46290 107.96774 2.847581  3.0032258     0.2920968        1.922097 5.453548 1.0654839 3.163387
# 3       3 13.13412 3.307255 2.417647   21.24118  98.66667 1.683922  0.8188235     0.4519608        1.145882 7.234706 0.6919608 1.696667
# Proline
# 1  510.1692
# 2 1100.2258
# 3  619.0588

ct.km <- table(wine$Type, fit.km$cluster) # 정오분류표
#    1  2  3
# 1  0 59  0
# 2 65  3  3
# 3  0  0 48

# `randIndex()` 함수를 이용하면 실제 와인의 종류(Type)와 군집간의 일치도(agreement)를 나타내는 수정된 순위 지수를 구할 수 있다
# 수정된의 의미는 우연에 의해 발생되는 경루를 고려한 값이며, -1(no agreement)과 1(perfect agreement) 사이의 값을 가진다
# install.packages("flexclust")
library(flexclust)
randIndex(ct.km)
#      ARI 
# 0.897495


# 3. 혼합 분포 군집 -------------------------------------------------------------
# Model-based 군집 방법, k의 모수적 모형의 가중합으로 표현되는 모집단 모형
# 모수와 함께 가중치를 자료로부터 추정하는 방법
# 흔히 사용되는 알고리즘 : EM-알고리즘

install.packages("mixtools")
library(mixtools)

data("faithful")

hist(faithful$waiting, cex.main=1.5, cex.lab=1.5, cex.axis=1.4,
     main="Time between Old Faithful eruptions", xlab="Minutes", ylab="")

wait1 <- normalmixEM(faithful$waiting, lambda = .5, mu = c(55,80), sigma=5)
# number of iterations= 9 

summary(wait1)
# summary of normalmixEM object:
# comp 1   comp 2
# lambda  0.36085  0.63915
# mu     54.61364 80.09031
# sigma   5.86909  5.86909
# loglik at estimate:  -1034.002 

plot(wait1, density=TRUE, cex.axis=1.4, cex.lab=1.4, cex.main=1.8,
     main2="Time between Old Faithful eruptions", xlab2="minutes")
# 해석 예 : EM알고리즘을 통해 모수를 추정하는 과정에서 반복횟수 2회 만에 로그-가능도 함수가 최대가 됨을 알 수 있다.

# dataset : iris
library(mclust)

mc <- Mclust(iris[,1:4], G=3)
summary(mc, parameters = TRUE) # 혼합분포의 모수추정치와 요약 결과 출력
# Gaussian finite mixture model fitted by EM algorithm 
#   
#   Mclust VEV (ellipsoidal, equal shape) model with 3 components: 
#   
#   log-likelihood   n df       BIC       ICL
# -186.074 150 38 -562.5522 -566.4673
# 
# Clustering table:
#   1  2  3 
# 50 45 55 
# 
# Mixing probabilities:
#   1         2         3 
# 0.3333333 0.3005423 0.3661243 
# 
# Means:
#   [,1]     [,2]     [,3]
# Sepal.Length 5.006 5.915044 6.546807
# Sepal.Width  3.428 2.777451 2.949613
# Petal.Length 1.462 4.204002 5.482252
# Petal.Width  0.246 1.298935 1.985523
# 
# Variances:
#   [,,1]
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# Sepal.Length   0.13320850  0.10938369  0.019191764 0.011585649
# Sepal.Width    0.10938369  0.15495369  0.012096999 0.010010130
# Petal.Length   0.01919176  0.01209700  0.028275400 0.005818274
# Petal.Width    0.01158565  0.01001013  0.005818274 0.010695632
# [,,2]
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# Sepal.Length   0.22572159  0.07613348   0.14689934  0.04335826
# Sepal.Width    0.07613348  0.08024338   0.07372331  0.03435893
# Petal.Length   0.14689934  0.07372331   0.16613979  0.04953078
# Petal.Width    0.04335826  0.03435893   0.04953078  0.03338619
# [,,3]
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# Sepal.Length   0.42943106  0.10784274   0.33452389  0.06538369
# Sepal.Width    0.10784274  0.11596343   0.08905176  0.06134034
# Petal.Length   0.33452389  0.08905176   0.36422115  0.08706895
# Petal.Width    0.06538369  0.06134034   0.08706895  0.08663823

plot.Mclust(mc) # 군집 결과 시각화

str(mc)
mc$classification # 분류 결과 확인
# [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3
# [70] 2 3 2 3 2 2 2 2 3 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
# [139] 3 3 3 3 3 3 3 3 3 3 3 3

predict(mc, data=iris[,2]) # 새로운 데이터 분류

# 혼합분포군집은 k-means의 절차와 유사하나 확률분포를 도입하여 군집을 수행하는 모형-기반의 군집 방법
# 군집을 몇 개의 모수로 표현할 수 있으며, 서로 다른 크기나 모양의 군집을 찾을 수 있다
# EM알고리즘을 이용한 모수추정에서 데이터가 커지면 수렴하는데 시간이 걸릴 수 있으며, 군집의 크기가 너무 작으면 추정의 정도가 떨어지거나 어려울 수 있다
# k-means과 마찬가지로 이상값 자료에 민감하므로 사전에 조치가 필요하다


# 4. SOM ------------------------------------------------------------------
# 자기조직화지도 알고리즘 = 코호넨 맵 이라고도 함
# 비지도 신경망으로 고차원의 데이터를 이해하기 쉬운 저차원의 뉴런으로 정렬하여 지도의 형태로 형상화

install.packages("kohonen")
library(kohonen)

data(wines)
str(wines)
#num [1:177, 1:13] 13.2 13.2 14.4 13.2 14.2 ...
# - attr(*, "dimnames")=List of 2
# ..$ : NULL
# ..$ : chr [1:13] "alcohol" "malic acid" "ash" "ash alkalinity" ...

head(wines)
#      alcohol malic acid  ash ash alkalinity magnesium tot. phenols flavonoids non-flav. phenols proanth col. int. col. hue OD ratio proline
# [1,]   13.20       1.78 2.14           11.2       100         2.65       2.76              0.26    1.28      4.38     1.05     3.40    1050
# [2,]   13.16       2.36 2.67           18.6       101         2.80       3.24              0.30    2.81      5.68     1.03     3.17    1185
# [3,]   14.37       1.95 2.50           16.8       113         3.85       3.49              0.24    2.18      7.80     0.86     3.45    1480
# [4,]   13.24       2.59 2.87           21.0       118         2.80       2.69              0.39    1.82      4.32     1.04     2.93     735
# [5,]   14.20       1.76 2.45           15.2       112         3.27       3.39              0.34    1.97      6.75     1.05     2.85    1450
# [6,]   14.39       1.87 2.45           14.6        96         2.50       2.52              0.30    1.98      5.25     1.02     3.58    1290

wines.sc <- scale(wines)
set.seed(7)

# wine.som <- som(data=wines.sc, grid=somgrid(5, 4, "hexagonal"),
#                 rlen = 100, alpha = c(0.05, 0.01), toroidal = FALSE, keep.data = TRUE)
wine.som <- som(wines.sc, grid = somgrid(5, 4, "hexagonal"), rlen = 100, alpha = c(0.05, 0.01), keep.data=TRUE)
wine.som_1 <- som(wines.sc, grid = somgrid(5, 4, "hexagonal"), rlen = 500, alpha = c(0.05, 0.01), keep.data=TRUE)

par(mfrow = c(1,2))
plot(wine.som, type="changes", main="Wine data: SOM(Learning no=100")
plot(wine.som_1, type="changes", main="Wine data: SOM(Learning no=500")

summary(wine.som)
# SOM of size 5x4 with a hexagonal topology and a bubble neighbourhood function.
# The number of data layers is 1.
# Distance measure(s) used: sumofsquares.
# Training data included: 177 objects.
# Mean distance to the closest unit in the map: 3.783.

par(mfrow = c(1,1))
plot(wine.som, type="counts", main="wine data: counts")
plot(wine.som, type="quality", main="wine data: mapping quality")
# plot(wine.som, type="mapping", labels=wine.classes, col=wine.classes, main="mapping plot")


# 5. Density-based Clustering ---------------------------------------------

# install.packages("fpc")
library(fpc)

newiris <- iris[-5]
ds <- fpc::dbscan(newiris, eps=0.42, MinPts=5)

ds
# dbscan Pts=150 MinPts=5 eps=0.42
#         0  1  2  3
# border 29  6 10 12
# seed    0 42 27 24
# total  29 48 37 36

table(ds$cluster, iris$Species)
#     setosa versicolor virginica
# 0      2         10        17
# 1     48          0         0
# 2      0         37         0
# 3      0          3        33

ds1 <- fpc::dbscan(newiris, eps=0.42, MinPts=10)
table(ds1$cluster, iris$Species)
#     setosa versicolor virginica
# 0      6         25        50
# 1     44          0         0
# 2      0         10         0
# 3      0         15         0

ds2 <- fpc::dbscan(newiris, eps=0.5, MinPts=5)
table(ds2$cluster, iris$Species)
#     setosa versicolor virginica
# 0      1          6        10
# 1     49          0         0
# 2      0         44        40

plot(ds, newiris)

newiris[c(1,4)]
plot(ds, newiris[c(1,4)])

plotcluster(newiris, ds$cluster)


# 6. Fuzzy Clustering -----------------------------------------------------

# https://rpubs.com/rahulSaha/Fuzzy-CMeansClustering
library(cluster)

data(iris)
x <- iris[,-5]
x[1:5,]

pairs(x, col=iris[,5])

cor(iris[,1:4])
#              Sepal.Length Sepal.Width Petal.Length Petal.Width
# Sepal.Length    1.0000000  -0.1175698    0.8717538   0.8179411
# Sepal.Width    -0.1175698   1.0000000   -0.4284401  -0.3661259
# Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654
# Petal.Width     0.8179411  -0.3661259    0.9628654   1.0000000

require(psych)
pairs.panels(iris[,-5], method = "pearson")  

# install.packages("ppclust")
require(ppclust)
res.fcm <- fcm(x, centers=3)

as.data.frame(res.fcm$u)[1:6,]
#     Cluster 1   Cluster 2 Cluster 3
# 1 0.002304380 0.001072034 0.9966236
# 2 0.016649509 0.007497947 0.9758525
# 3 0.013759500 0.006414579 0.9798259
# 4 0.022465031 0.010107523 0.9674274
# 5 0.003761709 0.001767935 0.9944704
# 6 0.044806233 0.020619654 0.9345741

res.fcm$v0
#           Sepal.Length Sepal.Width Petal.Length Petal.Width
# Cluster 1          4.9         2.5          4.5         1.7
# Cluster 2          6.7         3.3          5.7         2.1
# Cluster 3          5.0         3.2          1.2         0.2

res.fcm$v
#           Sepal.Length Sepal.Width Petal.Length Petal.Width
# Cluster 1     5.888932    2.761069     4.363952   1.3973150
# Cluster 2     6.775011    3.052382     5.646782   2.0535467
# Cluster 3     5.003966    3.414089     1.482816   0.2535463

summary(res.fcm)
# Summary for 'res.fcm'
# 
# Number of data objects:  150 
# 
# Number of clusters:  3 
# 
# Crisp clustering vector:
#   [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [70] 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 2 2 2 2 1 2 2 2 2 2 2 1 2 2 2 2 2 1 2 1 2 1 2 2 1 1 2 2 2 2 2 1 2 2 2 2
# [139] 1 2 2 2 1 2 2 2 1 2 2 1
# 
# Initial cluster prototypes:
#   Sepal.Length Sepal.Width Petal.Length Petal.Width
# Cluster 1          4.9         2.5          4.5         1.7
# Cluster 2          6.7         3.3          5.7         2.1
# Cluster 3          5.0         3.2          1.2         0.2
# 
# Final cluster prototypes:
#   Sepal.Length Sepal.Width Petal.Length Petal.Width
# Cluster 1     5.888932    2.761069     4.363952   1.3973150
# Cluster 2     6.775011    3.052382     5.646782   2.0535467
# Cluster 3     5.003966    3.414089     1.482816   0.2535463
# 
# Distance between the final cluster prototypes
# Cluster 1 Cluster 2
# Cluster 2  2.946292          
# Cluster 3 10.818752 23.846049
# 
# Difference between the initial and final cluster prototypes
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# Cluster 1  0.988932360   0.2610694  -0.13604836 -0.30268496
# Cluster 2  0.075011224  -0.2476177  -0.05321822 -0.04645334
# Cluster 3  0.003965961   0.2140889   0.28281553  0.05354632
# 
# Root Mean Squared Deviations (RMSD): 0.6725337 
# Mean Absolute Deviation (MAD): 3.553936 
# 
# Membership degrees matrix (top and bottom 5 rows): 
#   Cluster 1   Cluster 2 Cluster 3
# 1 0.002304380 0.001072034 0.9966236
# 2 0.016649509 0.007497947 0.9758525
# 3 0.013759500 0.006414579 0.9798259
# 4 0.022465031 0.010107523 0.9674274
# 5 0.003761709 0.001767935 0.9944704
# ...
# Cluster 1 Cluster 2  Cluster 3
# 146 0.1063871 0.8823507 0.01126223
# 147 0.5075252 0.4666788 0.02579593
# 148 0.1564396 0.8314467 0.01211367
# 149 0.1890364 0.7893823 0.02158126
# 150 0.5817811 0.3913000 0.02691888
# 
# Descriptive statistics for the membership degrees by clusters
# Size       Min        Q1      Mean    Median        Q3       Max
# Cluster 1   60 0.5075252 0.6697398 0.7826035 0.7963157 0.9164202 0.9737972
# Cluster 2   40 0.5006317 0.7807561 0.8351480 0.8604619 0.9122633 0.9888134
# Cluster 3   50 0.8413450 0.9541261 0.9645018 0.9763228 0.9850474 0.9995473
# 
# Dunn's Fuzziness Coefficients:
# dunn_coeff normalized 
#  0.7833975  0.6750962 
# 
# Within cluster sum of squares by cluster:
#        1        2        3 
# 36.81767 27.05750 15.15100 
# (between_SS / total_SS =  88.04%) 
# 
# Available components: 
#  [1] "u"          "v"          "v0"         "d"          "x"          "cluster"    "csize"      "sumsqrs"    "k"          "m"         
# [11] "iter"       "best.start" "func.val"   "comp.time"  "inpargs"    "algorithm"  "call"

