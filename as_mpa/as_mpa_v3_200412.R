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
  filter(str_detect(response, "^[^>]+[A-Za-z\\d]") | response == "") %>% 
  mutate(response = lemmatize_strings(cleaned_text$response))

# Q1
q1 <- cleaned_text %>% 
  filter(question == 'Q1') %>% 
  mutate(response = tolower(response)) %>% 
  mutate(mispicks = ifelse(str_detect(response, 'mispick'), 1, 0)) %>% 
  mutate(underpicks = ifelse(str_detect(response, 'under|uner'), 1, 0)) %>% 
  mutate(overpicks = ifelse(str_detect(response, 'over'), 1, 0)) %>% 
  mutate(differentdealer = ifelse(str_detect(response, 'dealer'), 1, 0)) %>% 
  mutate(etc = ifelse(str_detect(response, 'mispick|under|uner|over|dealer'), 0, 1)) 

q1 %>% 
  count(mispicks, underpicks, overpicks, differentdealer, etc, sort=TRUE)
## A tibble: 15 x 6
#    mispicks underpicks overpicks differentdealer   etc     n
#       <dbl>      <dbl>     <dbl>           <dbl> <dbl> <int>
#  1        0          0         0               1     0   301
#  2        0          0         0               0     1   246
#  3        1          0         0               0     0   138
#  4        0          1         0               0     0    71
#  5        1          0         0               1     0    63
#  6        0          0         1               0     0    17
#  7        0          1         0               1     0    14
#  8        0          0         1               1     0     5
#  9        1          1         0               0     0     5
# 10        1          1         0               1     0     3
# 11        1          1         1               1     0     3
# 12        0          1         1               0     0     2
# 13        1          0         1               0     0     2
# 14        1          0         1               1     0     2
# 15        1          1         1               0     0     1

# Q2
q2 <- cleaned_text %>% 
  filter(question == 'Q2') %>% 
  unnest_tokens(word, response) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

q2 %>%
  count(word, sort = TRUE) %>%
  print(n=1000)
 
#S1
cps <- Corpus(VectorSource(q2_tdm$response))
# (tdm <- TermDocumentMatrix(cps))
# tdm <- TermDocumentMatrix(corpus,control=list(removePunctuation=T))
tdm <- TermDocumentMatrix(cps,control=list(
  removePunctuation=T,
  wordLengths=c(1,Inf),
  removeNumbers=T,
  stopwords=stopwords("SMART")))
  # weighting = function(x) weightTfIdf(x, normalize=TRUE)))

t(inspect(tdm[1:30, 1:20]))
tdm_matrix <- as.data.frame(t(as.matrix(tdm)))
write_csv(tdm_matrix, "tdm_matrix.csv",)

# Q3
q3 <- cleaned_text %>% 
  filter(question == 'Q3') %>% 
  unnest_tokens(word, response) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

q3 %>%
  count(word, sort = TRUE) %>%
  print(n=1000)
 
#S1
cps <- Corpus(VectorSource(q2_tdm$response))
# (tdm <- TermDocumentMatrix(cps))
# tdm <- TermDocumentMatrix(corpus,control=list(removePunctuation=T))
tdm <- TermDocumentMatrix(cps,control=list(
  removePunctuation=T,
  wordLengths=c(1,Inf),
  removeNumbers=T,
  stopwords=stopwords("SMART")))
  # weighting = function(x) weightTfIdf(x, normalize=TRUE)))

t(inspect(tdm[1:30, 1:20]))
tdm_matrix <- as.data.frame(t(as.matrix(tdm)))
write_csv(tdm_matrix, "tdm_matrix.csv",)
