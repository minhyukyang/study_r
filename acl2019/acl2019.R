library(httr)
library(rvest)
library(dplyr)

url = "http://schedule.acl2019.org/"

paper_list <- read_html(url) %>% html_nodes(xpath = '//*[@class="paper-card"]') 

result <- data.frame(id=NULL,title=NULL,authors=NULL,abstract=NULL,paperlink=NULL)
result_tmp <- data.frame(id=NULL,title=NULL,authors=NULL,abstract=NULL,paperlink=NULL)

#csv
for (i in 1:length(paper_list)){
# for (i in 1:10){
  tmp_list = paper_list[i]
  tmp_id = tmp_list %>% html_attr('id')
  tmp_title = tmp_list %>% html_node('.title') %>% html_text
  tmp_authors = tmp_list %>% html_node('.authors') %>% html_text
  tmp_abstract = tmp_list %>% html_node('.abstract') %>% html_text
  tmp_paperlink = tmp_list %>% html_node('.links') %>% html_node('.paper') %>% html_attr('href')
  
  result_tmp = data.frame(id=tmp_id,title=tmp_title,authors=tmp_authors,abstract=tmp_abstract,paperlink=tmp_paperlink)
  result = rbind(result, result_tmp)
  print(i)
}
# next : download files on disk

# lists
write.csv(result, file = "acl2019.csv")
