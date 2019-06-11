##### 4장 : 나이브 베이즈(Naive Bayes)를 사용한 분류 --------------------

## 예제 : 스팸 SMS 메시지 제거 ----
## 2 단계 : 데이터 준비와 살펴보기  ---- 

# sms 데이터 프레임으로 sms 데이터 읽기
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)

# sms 데이터 구조
str(sms_raw)

# 팩터로 spam/ham으로 변환
sms_raw$type <- factor(sms_raw$type)

# 변수형 확인
str(sms_raw$type)
table(sms_raw$type)

# 텍스트 마이닝(tm) 패키지를 사용하여 말뭉치 생성
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))

# sms 말뭉치 확인
print(sms_corpus)
inspect(sms_corpus[1:3])

# tm_map() 사용하여 말뭉치 정리
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

# 말뭉치 정리 확인
inspect(sms_corpus[1:3])
inspect(corpus_clean[1:3])

# 문서-용어 희소 매트릭스 생성
sms_dtm <- DocumentTermMatrix(corpus_clean)
sms_dtm

# 훈련과 테스트 데이터셋 생성
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test  <- sms_raw[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]

# 스팸 비율 확인
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# 단어 클라우드 시각화
library(wordcloud)

wordcloud(sms_corpus_train, min.freq = 30, random.order = FALSE)

# 훈련 데이터를 스팸과 햄으로 구분
spam <- subset(sms_raw_train, type == "spam")
ham  <- subset(sms_raw_train, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

# 빈번한 단어에 대한 속성 지시자
findFreqTerms(sms_dtm_train, 5)
sms_dict <- Dictionary(findFreqTerms(sms_dtm_train, 5))
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

# 개수를 팩터로 변환
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts()를 사용한 훈련/테스트 데이터 추출
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)

## 3 단계 : 데이터로 모델 훈련 ----
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

## 4 단계 : 모델 성능 평가 ----
sms_test_pred <- predict(sms_classifier, sms_test)

library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

## 5 단계 : 모델 성능 향상 ----
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
