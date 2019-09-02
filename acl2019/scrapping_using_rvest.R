# Extract list and abstract for ACL2019 papers

library(rvest)
library(dplyr)

# 1. URL ####
url <- "http://schedule.acl2019.org/#"


read_html(url) %>% html_nodes(xpath = '//*[@class="abstract" or @class="title" or @class="authors" or @class="paper-card"]')

# acl_paper_cards <- 
read_html(url) %>% html_nodes(xpath = '//*[@class="paper-card"]') %>% html_attr('id')



# tvcast list
url_acl = "http://schedule.acl2019.org/#"
html_acl = read_html(url_acl, encoding="utf-8")



# 1.2 Scrapping URLs ####
library(rvest)
library(dplyr)

temp <- unique(html_nodes(html, '#main_pack') %>%
                 html_nodes(css='.news.mynews.section._prs_nws') %>%
                 html_nodes(css='.type01') %>%
                 html_nodes('a') %>%
                 html_attr('href'))

news_url <- c()
news_date <- c()

for (dt in date){
  for (page_num in page){
    naver_url <- paste0(naver_url_1,search_keyword,naver_url_2,dt,naver_url_3,page_num)
    html_naver <- read_html(naver_url)
    temp <- unique(html_nodes(html_naver, '#main_pack') %>%
                     html_nodes(css='.news.mynews.section._prs_nws') %>%
                     html_nodes(css='.type01') %>%
                     html_nodes('a') %>%
                     html_attr('href'))
    news_url <- c(news_url,temp)
    news_date <- c(news_date,rep(dt,length(temp)))
    print(c(dt,page_num))
    
  }
}

# 1.3 Scrapping contents ####
news_content <- c()
news_url <- news_url[grep("http", news_url)] %>% unique(news_url)

try({
  for (i in 1:length(news_url)){
    print(news_url[i])
    
    if(substr(news_url[i],1,2) == "ht"){
      html <- read_html(news_url[i])
      temp <- repair_encoding(html_text(html_nodes(html,'#news_contents')), from='UTF-8')
      news_content <- c(news_content,temp)  
      
    } else {
      next
    }
    
  }}
  , silent=TRUE)

news <- cbind(date=news_date,url=news_url,content=unlist(news_content))
news <- as.data.frame(news)

# Ex. naver tvcast - extract list ####
# library(rvest)

# tvcast list
url_tvcast = "http://tvcast.naver.com/jtbc.youth"
html_tvcast = read_html(url_tvcast, encoding="utf-8")

# extract a tag 'a' 
html_tvcast %>% html_nodes(".title a")
html_tvcast %>% html_nodes(".title a") %>% html_text()
html_tvcast %>% html_nodes(".title a") %>% html_text() %>% data.frame()

