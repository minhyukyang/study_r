# 4. 연관 분석

# 연관 규칙이란  항목들 간의 '조건-결과'식으로 표현되는 유용한 패턴
# ex) 연관 분석, 장바구니 분석

# 지지도(support) = 전체 거래 중에서 품목 A, B가 동시에 포함되는 거래 비율 = P(A n B)
# 신뢰도(confidence) = 품목 A가 포함된 거래 중에서 품목 A, B를 동시에 포함하는 거랭ㄹ 확률(연관성의 정도) = P(A n B)/P(A)
# 향상도(lift) = 품목 B를 구매한 고객 대비 품목 A를 구매한 후 품목 B를 구매하는 고객에 대한 확률 = P(B|A)/P(B) = P(A n B)/P(A)P(B)
#  -> 향상도가 1보다 크면 이 규칙은 결과를 예측하는 데 있어서 우수하다고 판단

# install.packages("arules")
library(arules)

data(Adult)
Adult
# transactions in sparse format with
# 48842 transactions (rows) and
# 115 items (columns)

rules <- apriori(Adult)
# Apriori
# 
# Parameter specification:
#   confidence minval smax arem  aval originalSupport maxtime support minlen maxlen target  ext
# 0.8    0.1    1 none FALSE            TRUE       5     0.1      1     10  rules TRUE
# 
# Algorithmic control:
#   filter tree heap memopt load sort verbose
# 0.1 TRUE TRUE  FALSE TRUE    2    TRUE
# 
# Absolute minimum support count: 4884 
# 
# set item appearances ...[0 item(s)] done [0.00s].
# set transactions ...[115 item(s), 48842 transaction(s)] done [0.04s].
# sorting and recoding items ... [31 item(s)] done [0.01s].
# creating transaction tree ... done [0.03s].
# checking subsets of size 1 2 3 4 5 6 7 8 9 done [0.11s].
# writing ... [6137 rule(s)] done [0.00s].
# creating S4 object  ... done [0.01s].

inspect(head(rules))
#     lhs                         rhs                            support   confidence coverage  lift     count
# [1] {}                       => {race=White}                   0.8550428 0.8550428  1.0000000 1.000000 41762
# [2] {}                       => {native-country=United-States} 0.8974243 0.8974243  1.0000000 1.000000 43832
# [3] {}                       => {capital-gain=None}            0.9173867 0.9173867  1.0000000 1.000000 44807
# [4] {}                       => {capital-loss=None}            0.9532779 0.9532779  1.0000000 1.000000 46560
# [5] {relationship=Unmarried} => {capital-loss=None}            0.1019819 0.9719024  0.1049302 1.019537  4981
# [6] {occupation=Sales}       => {race=White}                   0.1005282 0.8920785  0.1126899 1.043314  4910

adult.rules <- apriori(Adult, parameter=list(support=0.1, confidence=0.6),
                                             appearance = list(rhs=c('income=small', 'income=large'), default='lhs'),
                                             control=list(verbose=F))
adult.rules.sorted <- sort(adult.rules, by='lift')
inspect(head(adult.rules.sorted))
#     lhs                                                                             rhs            support   confidence coverage  lift     count
# [1] {age=Young,workclass=Private,capital-loss=None}                              => {income=small} 0.1005282 0.6633342  0.1515499 1.310622 4910 
# [2] {age=Young,workclass=Private}                                                => {income=small} 0.1025961 0.6630938  0.1547234 1.310147 5011 
# [3] {age=Young,marital-status=Never-married,capital-gain=None,capital-loss=None} => {income=small} 0.1060563 0.6616426  0.1602924 1.307279 5180 
# [4] {age=Young,marital-status=Never-married,capital-gain=None}                   => {income=small} 0.1084517 0.6609683  0.1640801 1.305947 5297 
# [5] {relationship=Own-child,capital-loss=None}                                   => {income=small} 0.1000983 0.6604972  0.1515499 1.305016 4889 
# [6] {relationship=Own-child}                                                     => {income=small} 0.1023914 0.6596755  0.1552148 1.303393 5001 

# install.packages("arulesViz")
library(arulesViz)
plot(adult.rules.sorted, method="scatterplot")
plot(adult.rules.sorted, method="graph", control=list(type='items', alpha=0.5))
