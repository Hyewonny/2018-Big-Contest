library(rvest)

getwd()
setwd("C:/Users/user/Desktop")
names <- read.csv("2005-2017 100만이상 영화.csv", header=T, stringsAsFactors = F)

i<-62
new_title <- c()
for (i in 1:nrow(names)){
  new_title[i] <- gsub(" ","+",names$title1[i])
}
new <- data.frame(new_title)
new1<- NULL
##영화 제목이 있는 엑셀파일로 url 만들기##
for (i in 1:nrow(names)){
  
  name <- paste0("영화+", new[i,1]) 
  if (i==62) {name <- "가문의+위기-가문의+영광2"}
  name <- gsub("&","%26",name)
  url <- "https://search.naver.com/search.naver?sm=tab_hty.top&where=nexearch&query="
  url <- paste0(url, name)
  
  ##만든 url에 접근해 정보를 가져옴##
  htm <- read_html(url) #url에 접근
  node <- html_nodes(htm, ".sh_movie_link")         #htm이라는 url에 저장되어 있는 node에 접근.
  movie_site <- html_attr(node, 'href')        #접근한 node에서 사이트가 뽑혀 나온다.
  movie_site <- gsub("basic", "detail", movie_site) 
  
  if(length(movie_site)==0) { name <- substr(name, 4,100)
  #영화 고사두번째이야기 이런거는 앞에 영화를 붙이면 검색이 안되는 경우가 있죠;
  #그래서 검색이 안되면 movie_site가 character(0)로 나와서 
  #"영화+"을 제거해주고 그 다음 영화 제목만을 가져오려고 코드 추가해준겁니다. 그다음과정은 위와 같습니다.
  url <- "https://search.naver.com/search.naver?sm=tab_hty.top&where=nexearch&query="
  url <- paste0(url, name)
  
  htm <- read_html(url)
  node <- html_nodes(htm, ".sh_movie_link")
  
  movie_site <- html_attr(node, 'href')
  movie_site <- gsub("basic", "detail", movie_site)}#movie_site안에 있는 url의 basic을 detail로 바꾼다.
  
   
  htm1 <- read_html(movie_site)
  
  node1 <- html_nodes(htm1, ".agency_name dd:nth-child(6)")
  agency <- html_text(node1)
  if (length(agency)==0){
    node1<-html_nodes(htm1,".agency_name dd:nth-child(4) a")
    agency<-html_text(node1)}
  # 배급사가 따로 없을 때에는 제작사를 가져오기로 합니다.
  else if (length(agency)==0) {
    node1 <- html_nodes(htm1,".agency_name dd:nth-child(2) a")
    agency<-html_text(node1)
  }
    print(i)
    new1<- append(new1,agency)
}  
agency
