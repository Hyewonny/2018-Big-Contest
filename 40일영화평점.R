rm(list=ls())

library(rvest)
library(stringr)

getwd()
setwd("C:\\Users\\kimhs\\OneDrive\\바탕 화면\\movie\\data")
movien <- read.csv("40days.csv", header=T, stringsAsFactors = F)
names <- as.data.frame(movien[,1])

i = 1
grade_df <- NULL
for(i in 1:nrow(names)) { 
  name <- paste0("영화+",names[i,1])
  name <- gsub(" ","", name)
  name <- gsub("&","%26", name) 
  name <- gsub(":","", name)
  name <- gsub(";","", name)
  name <- gsub("~","", name)
  name <- gsub("!","", name)
  name <- gsub("-","", name)
  name <- gsub(",","", name)
  #name <- gsub("\\.","", name)
  if(name == "영화+써커펀치") {name <- "써커펀치"}
  url <- "https://search.naver.com/search.naver?sm=tab_hty.top&where=nexearch&query="
  url <- paste0(url, name)
  htm <- read_html(url) 
  
  node <- html_nodes(htm, ".sh_movie_link") 
  if(length(node) == 0) {
    name <- paste0(names[i,1])
    name <- gsub(" ","", name)
    name <- gsub("&","%26", name) 
    name <- gsub(":","", name)
    name <- gsub(";","", name)
    name <- gsub("~","", name)
    name <- gsub("!","", name)
    name <- gsub("-","", name)
    name <- gsub(",","", name)
    url <- "https://search.naver.com/search.naver?sm=tab_hty.top&where=nexearch&query="
    url <- paste0(url, name)
    htm <- read_html(url)
    node <- html_nodes(htm, ".sh_movie_link") 
  }
  movie_site <- html_attr(node, 'href')
  htm1 <- read_html(movie_site)
  
  ## 평점
  node1 <- html_nodes(htm1, "#pointNetizenPersentWide")
  grade <- as.numeric(html_text(node1))
  print(c(round(i,digit = 0),grade))
  grade_df <- append(grade_df, grade)
}



movie <- read.csv("mov_data_40_final.csv", header=T, stringsAsFactors = F)
movie$Netizen_score <- grade_df
