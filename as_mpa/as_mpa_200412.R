# https://www.kirenz.com/post/2019-09-16-r-text-mining/
# https://github.com/trinker/textstem


# 0. Set the environment --------------------------------------------------

# setwd("D:/Analysis/R/as_mpa")
setwd("as_mpa/")

library(tm)
library(tidyverse)
library(textstem)

# UDF
text_clean <- function(x) {
  x <- tolower(x)
  x <- removeWords(stopwords('en'))
  x <- removePunctuation(x)
  x <- removeNumbers(x)
  x <- stripWhitespace(x)
}

# 1. Data Load ------------------------------------------------------------

mpa_raw <- as_tibble(read_csv("mpa.csv"))

# split the data
question_list <- mpa_raw$`Question Text`[!duplicated(mpa_raw$`Question Text`)]
q1_1 <- mpa_raw[mpa_raw$`Question Text` == question_list[1],]
# q1_1_response <- q1_1$Response
 
q1_1_text <- as_tibble(q1_1_response) %>% mutate(document = row_number())

lemmatize_strings(q1_1_text$value)

# Tokenization
library(tidytext)

tidy_texts <- q1_1_text %>%
  unnest_tokens(word, value) %>%
  group_by(word) %>%
  filter(n() > 10) %>%
  ungroup()

# Stopwords
library(stopwords)

stopword <- as_tibble(stopwords::stopwords("en")) 
stopword <- rename(stopword, word=value)
tb <- anti_join(tidy_texts, stopword, by = 'word')

before_vs_after <- function(sent_id){
  
  bf <- tb %>%
    filter(document == sent_id) %>%
    .$word %>%
    str_c(collapse = " ")
  
  af <- tb %>%
    filter(document == sent_id) %>%
    .$word %>%
    str_c(collapse = " ") %>%
    lemmatize_strings()
  
  print(str_c("Before:", bf, sep = " ", collapse = " "))
  print(str_c("After :", af, sep = " ", collapse = " "))
}

before_vs_after(9)

 # textclean ---------------------------------------------------------------
# https://rpubs.com/WulanAndriyani/TextPreprocessing
install.packages("textclean")
library(textclean)

check_text(q1_1$Response)

tb_proces <- q1_1 %>% 
  drop_empty_row() %>% 
  drop_NA()

tb_proces <- as.character(q1_1$Response)

tb_proces <- replace_contraction(tb_proces)

tb_proces <- replace_date(tb_proces, replacement = "")

# tb_proces <- replace_email(tb_proces)
# tb_proces <- replace_emoji(tb_proces)
# tb_proces <- replace_grade(tb_proces)
# tb_proces <- replace_hash(tb_proces)

tb_proces <- replace_incomplete(tb_proces, replacement = "")

tb_proces <- replace_number(tb_proces, remove = TRUE)

tb_proces <- replace_word_elongation(tb_proces)

library(tokenizers)

tb_proces <- tb_proces %>% 
  tokenize_words() %>% 
  as.character()

head(tb_proces)

# test ---------------------------------------------------------------------
# https://www.mjdenny.com/Text_Processing_In_R.html

my_string <- "Example STRING, with example numbers (12, 15 and also 10.2)?!"
lower_string <- tolower(my_string)

second_string <- "Wow, two sentences."
my_string <- paste(my_string,second_string,sep = " ")

my_string_vector <- str_split(my_string, "!")[[1]]

grep("\\?",my_string_vector)
str_replace_all(my_string, "e","___")
str_extract_all(my_string,"[0-9]+")

Clean_String <- function(string){
  # Lowercase
  temp <- tolower(string)
  # Remove everything that is not a number or letter (may want to keep more 
  # stuff in your actual analyses). 
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  # Split it
  temp <- stringr::str_split(temp, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  return(temp)
}

sentence <- "The term 'data science' (originally used interchangeably with 'datalogy') has existed for over thirty years and was used initially as a substitute for computer science by Peter Naur in 1960."
clean_sentence <- Clean_String(sentence)
clean_sentence

# function to clean text
Clean_Text_Block <- function(text){
  # Get rid of blank lines
  indexes <- which(text == "")
  if (length(indexes) > 0) {
    text <- text[-indexes]
  }
  # See if we are left with any valid text:
  if (length(text) == 0) {
    cat("There was no text in this document! \n")
    to_return <- list(num_tokens = 0, 
                      unique_tokens = 0, 
                      text = "")
  } else {
    # If there is valid text, process it.
    # Loop through the lines in the text and combine them:
    clean_text <- NULL
    for (i in 1:length(text)) {
      # add them to a vector 
      clean_text <- c(clean_text, Clean_String(text[i]))
    }
    # Calculate the number of tokens and unique tokens and return them in a 
    # named list object.
    num_tok <- length(clean_text)
    num_uniq <- length(unique(clean_text))
    to_return <- list(num_tokens = num_tok, 
                      unique_tokens = num_uniq, 
                      text = clean_text)
  }
  
  return(to_return)
}

Clean_Text_Block(q1_1$Response)
# q1_1_corpus <- Corpus(VectorSource(q1_1_response))
# # <<SimpleCorpus>>
# # Metadata:  corpus specific: 1, document level (indexed): 0
# # Content:  documents: 875
# 
# inspect(q1_1_corpus[1])
# # <<SimpleCorpus>>
# # Metadata:  corpus specific: 1, document level (indexed): 0
# # Content:  documents: 1
# # 
# # [1] receive parts for different dealerships and manufacturer
# 
# headlines<-as_tibble(read.csv(url('https://raw.githubusercontent.com/kwartler/
#                      text_mining/master/all_3k_headlines.csv'),encoding='latin1'))
# 
# headline.clean<-function(x){
#   x<-tolower(x)
#   x<-removeWords(stopwords('en'))
#   x<-removePunctuation(x)
#   x<-removeNumbers(x)
#   x<-stripWhitespace(x)
# }


# tidytext ----------------------------------------------------------------
# https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html


