
library(rvest)
library(stringr)
install.packages("xlsx")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_151')
library(xlsx)
library(dplyr)


getwd()
setwd("C:/Users/user/Desktop/movie_data2")

#사이트 주소 붙이기
url_vec <- NULL
url <- "http://www.kobis.or.kr/kobis/business/stat/boxs/findDailyBoxOfficeList.do?loadEnd=0&searchType=excel&sSearchFrom="
s <- as.Date("2009-01-01")
e <- as.Date("2018-07-31")

date <- seq(from=s, to=e, by=1)
date <- as.character(date)
ch <- "&sSearchTo="
i <-1
#사이트 주소 정리
for (i in 1:length(date)) {
  url1 <- paste0(url, date[i], ch, date[i])
  url_vec <- append(url_vec, url1)
}
url_df <- as.data.frame(url_vec)

##사이트 크롤링 (다운)
j <-1
for (j in 1:length(date)) {
  b <- str_sub(as.character(url_df[j,1]), start=135, end=144)
  b <- paste0(b,".xls")
  a <- as.character(url_df[j,1])
  download.file(a, destfile = b, method='libcurl')
}

setwd("C:/Users/user/Desktop/movie_data2")

s <- as.Date("2009-01-01")
e <- as.Date("2018-07-31")
date <- seq(from=s, to=e, by=1)
date <- as.character(date)


day_df <- NULL
i<-120
for (i in 1:length(date)) {
  #다운 받은 파일 정리 (개봉일이 NA값인것들 처리 등등)
  
  read_date <- paste0(date[i], ".xlsx")
  day_list <- read.xlsx(read_date, sheetIndex = 1, encoding = 'UTF-8', rowIndex = 4:200, colClasses = "character")
  movie_data <- day_list[-1,]
  head(movie_data)
  movie_data[3]
  # 개봉일이 없는 거는 빼기 
  exc <- which(is.na(movie_data[3]))
  movie_data <- movie_data[-exc,]
  # 필요한 설명변수만 모음
  vec <- c(2,3,9,12,13,15,18,19,20,21,22)
  movie_data <- as.matrix(movie_data[,vec])
  movie_data <- gsub(" ", "", movie_data)
  movie_data <- as.data.frame(movie_data)
  colnames(movie_data) <- c("title", "release", "day_audience", "total_audience", "screen", "Nationality",
                            "distributor", "grade", "genre", "director", "actor")
  # date(날짜) 추가,개봉일로부터 몇일 
  movie_data$date <- date[i] #당일 날짜
  
  # day(개봉일로부터 얼마나 됬는지 처리)
  exc1 <- NULL
  rel <- as.Date(movie_data$release)# 개봉일
  dat <- as.Date(movie_data$date)# 날짜
  da <- as.Date(as.character(date[i]))
  d <- da-rel + 1 #날짜와 개봉일 빼서 저장 ,
  exc1 <- append(exc1, which(d > 40 | d < 1))
  # 시사회까지 해준 날짜까지 포함해서 1보다 작은거 제외  
  d <- d[-exc1]# 빼주기 
  movie_data <- movie_data[-exc1,]
  movie_data$day <- as.integer(d) # 며칠 됐는지 표시 해주기 위해서 day라는 변수 추가 
  
  # date를 바탕으로 요일 정하기,weekdays 요일 반환 
  movie_data$weekday <- weekdays(as.Date(movie_data$date))
  
  #휴일 정리 파일 읽어와서 휴일인지 아닌지 정리
  # 휴일일때 1 휴일이아닐때 0
  holiday_list <- read.csv("holiday_cal.csv")
  holiday <- NULL
  x<-1
  for (x in 1:length(movie_data$date)) {
    r <- which(as.Date(movie_data$date[x]) == as.Date(as.matrix(holiday_list[1])))
    holiday <- append(holiday, holiday_list[r,2])
    holiday
  }
  movie_data$holiday <- holiday
  head(movie_data)
  
  #날마다 정리된 데이터를 붙인다.
  day_df <- rbind(day_df, movie_data)
  print(i)#디버깅 용도로
}


# 붙인 데이터를 title별로 나열한다.
ex <- day_df[order(day_df$title),]

# : 문자 제거 영화별로 영화제목에 특수문자; 특수문자 없애주기
ex <- gsub(':', '', as.matrix(ex))
ex <- as.data.frame(ex)
write.csv(day_df, file="movie.csv", row.names = F)  # 정렬되지 않은 데이터
write.csv(ex, file="ex.csv", row.names = F)   #정렬된 데이터- 영화별로 


getwd()
setwd("C:/Users/user/Desktop/movie_data2")
real_data <- read.csv("ex_.csv", header=T)  #다시 정렬된 데이터를 불러온다 (Factor문제 때문)
mov_title <- levels(real_data$title)   # 제목이 어떤 것이 있는지 확인



# 영화 제목별로 따로따로 저장, 영화마다 파일을 다르게 만들어주려고 
# 폴더 안에 넣어주기 
setwd("C:/Users/user/Desktop/total_movie")
j <- 1
movie_name <- NULL
release_list <- NULL
for (j in 1:length(mov_title)) {
  mov_title[j] <- gsub("/|:", "", mov_title[j])
 
  mov_title[j] <- gsub("?", "", mov_title[j],fixed=TRUE)
  
  wh <- which(mov_title[j] == real_data$title)
  # 제목의 위치를 뽑아내고
  sub_df <- real_data[wh,]
  # 그 위치에있는 거만 뽑아내겠다 
  sub_df[1,3] <- sub_df[1,4]
  #첫째날 일별관객수를 total관객수로 바꾸겠다(시사회 인원 포함) 
  ti <- paste0(mov_title[j], ".csv")
  sub_df$day
  if (is.na(sub_df[40,1]) == F && sub_df[40,4] >100000 && sub_df[40,13] == 40) {
    sub_df <- sub_df[1:40,]
    #14번째날의 데이터가 없음 ,14번째까지만 추출
    #2주 후 관객수예측해야해서 14일까지만 추출 
    write.csv(sub_df, ti, row.names = F)
    
    movie_name <- append(movie_name, mov_title[j])
    sub_df[,2] <- as.character(sub_df[,2])# 개봉일에 대한 정보 ,
    release_list <- append(release_list, sub_df[1,2])}# 영화이름, 개봉일 생성 
  print(j)
}
length(movie_name)
movie_na <- data.frame(movie_name, release_list)
write.csv(movie_na, "movie_title.csv", row.names=F)





