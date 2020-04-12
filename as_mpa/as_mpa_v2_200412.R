# test2

# https://www.kirenz.com/post/2019-09-16-r-text-mining/
# https://github.com/trinker/textstem

# 0. Set the environment --------------------------------------------------

# setwd("D:/Analysis/R/as_mpa")
setwd("as_mpa/")

library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# 1. Data Load ------------------------------------------------------------

raw_text <- as_tibble(read_csv("mpa.csv"))
colnames(raw_text) <- c("section_name", "question", "response", "brand")
raw_text$id <- 1:nrow(raw_text)

q_list <- raw_text$question[!duplicated(raw_text$question)]
# raw_text$q_id <- ifelse(raw_text$question == q_list[1], 'Q1', 
#                         ifelse(raw_text$question == q_list[2], 'Q2', 'Q3'))
raw_text$question <- ifelse(raw_text$question == q_list[1], 'Q1',
                            ifelse(raw_text$question == q_list[2], 'Q2', 'Q3'))

library(ggplot2)

raw_text %>%
  group_by(question) %>%
  summarize(messages = n_distinct(id)) %>%
  ggplot(aes(question, messages)) +
  geom_col() +
  coord_flip()

# 2. Pre-processing text --------------------------------------------------

library(stringr)

cleaned_text <- raw_text %>%
  filter(str_detect(response, "^[^>]+[A-Za-z\\d]") | response == "")

cleaned_text$response <- lemmatize_strings(cleaned_text$response)

#S1
cps <- Corpus(VectorSource(cleaned_text$response))
# (tdm <- TermDocumentMatrix(cps))
# tdm <- TermDocumentMatrix(corpus,control=list(removePunctuation=T))
tdm <- TermDocumentMatrix(cps,control=list(
  removePunctuation=T,
  wordLengths=c(1,Inf),
  removeNumbers=T,
  stopwords=stopwords("SMART")))
  # weighting = function(x) weightTfIdf(x, normalize=TRUE)))

inspect(tdm[1:10, 1:10])
# myDtm <- t(as.matrix(tdm))

findFreqTerms(tdm,
              lowfreq = 50)

cosine_dist_mat[1,]
library(proxy)
wss <- dist(myDtm, method = "cosine")
wss_2 <- sapply(1:50, 
              function(k){kmeans(wss, k, nstart=50, iter.max = 15 )$tot.withinss})



#S2
library(tidytext)

usenet_words <- cleaned_text %>%
  unnest_tokens(word, response) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

# 3. Analysis -------------------------------------------------------------

#S1
usenet_words %>%
  count(word, sort = TRUE)

library(textstem)

#S2 
words_by_question <- usenet_words %>%
  count(question, word, sort = TRUE) %>%
  ungroup()

words_by_question

# 3.1 tf-idf --------------------------------------------------------------

#S1
tf_idf <- words_by_question %>%
  bind_tf_idf(word, question, n) %>%
  arrange(desc(tf_idf))

tf_idf

#S2
tf_idf %>%
  group_by(question) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = question)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ question, scales = "free") +
  ylab("tf-idf") +
  coord_flip()


# 3.2 Topic modeling ------------------------------------------------------

# include only words that occur at least 50 times
word_groups <- usenet_words %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup() %>%
  filter(word_total > 50)

# convert into a document-term matrix
# with document names such as sci.crypt_14147
mpa_dtm <- word_groups %>%
  unite(document, question, id) %>%
  count(document, word) %>%
  cast_dtm(document, word, n)

library(topicmodels)
mpq_lda <- LDA(mpa_dtm, k = 4, control = list(seed = 2016))

mpq_lda %>%
  tidy() %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered()

#
usenet_bigrams <- cleaned_text %>%
  unnest_tokens(bigram, response, token = "ngrams", n = 2)

usenet_bigram_counts <- usenet_bigrams %>%
  count(question, bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")
