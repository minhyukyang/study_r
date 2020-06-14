
# Missing Data Handling ---------------------------------------------------

# 데이터 로드
install.packages("Amelia")

data(freetrade, package="Amelia")

summary(freetrade)
# year        country              tariff           polity            pop                gdp.pc           intresmi          signed      
# Min.   :1981   Length:171         Min.   :  7.10   Min.   :-8.000   Min.   : 14105080   Min.   :  149.5   Min.   :0.9036   Min.   :0.0000  
# 1st Qu.:1985   Class :character   1st Qu.: 16.30   1st Qu.:-2.000   1st Qu.: 19676715   1st Qu.:  420.1   1st Qu.:2.2231   1st Qu.:0.0000  
# Median :1990   Mode  :character   Median : 25.20   Median : 5.000   Median : 52799040   Median :  814.3   Median :3.1815   Median :0.0000  
# Mean   :1990                      Mean   : 31.65   Mean   : 2.905   Mean   :149904501   Mean   : 1867.3   Mean   :3.3752   Mean   :0.1548  
# 3rd Qu.:1995                      3rd Qu.: 40.80   3rd Qu.: 8.000   3rd Qu.:120888400   3rd Qu.: 2462.9   3rd Qu.:4.4063   3rd Qu.:0.0000  
# Max.   :1999                      Max.   :100.00   Max.   : 9.000   Max.   :997515200   Max.   :12086.2   Max.   :7.9346   Max.   :1.0000  
#                                   NA's   :58       NA's   :2                                              NA's   :13       NA's   :3       
# fiveop          usheg       
# Min.   :12.30   Min.   :0.2558  
# 1st Qu.:12.50   1st Qu.:0.2623  
# Median :12.60   Median :0.2756  
# Mean   :12.74   Mean   :0.2764  
# 3rd Qu.:13.20   3rd Qu.:0.2887  
# Max.   :13.20   Max.   :0.3083  
# NA's   :18      

mydata <- freetrade

# 1) record 자체를 삭제하는 방법
mydata2 <- mydata[!complete.cases(mydata),]

nrow(mydata) # 171
nrow(mydata2) # 75

# psych::describe 사용하기

install.packages("psych")
library(psych)

describe(freetrade)
# vars   n         mean           sd      median     trimmed         mad         min          max        range  skew kurtosis
# year        1 171      1990.00         5.49     1990.00     1990.00        7.41     1981.00      1999.00        18.00  0.00    -1.23
# country*    2 171          NaN           NA          NA         NaN          NA         Inf         -Inf         -Inf    NA       NA
# tariff      3 113        31.65        21.22       25.20       28.48       16.16        7.10       100.00        92.90  1.28     0.93
# polity      4 169         2.91         5.55        5.00        3.38        4.45       -8.00         9.00        17.00 -0.78    -0.90
# pop         5 171 149904501.04 254569660.45 52799040.00 77879715.09 51793207.70 14105080.00 997515200.00 983410120.00  2.35     3.96
# gdp.pc      6 171      1867.28      2562.95      814.27     1258.15      710.11      149.50     12086.23     11936.73  2.35     5.19
# intresmi    7 158         3.38         1.57        3.18        3.29        1.61        0.90         7.93         7.03  0.56    -0.15
# signed      8 168         0.15         0.36        0.00        0.07        0.00        0.00         1.00         1.00  1.89     1.59
# fiveop      9 153        12.74         0.35       12.60       12.74        0.44       12.30        13.20         0.90  0.25    -1.57
# usheg      10 171         0.28         0.02        0.28        0.28        0.02        0.26         0.31         0.05  0.45    -0.92
# se
# year            0.42
# country*          NA
# tariff          2.00
# polity          0.43
# pop      19467428.49
# gdp.pc        195.99
# intresmi        0.13
# signed          0.03
# fiveop          0.03
# usheg           0.00


