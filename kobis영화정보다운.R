install.packages("rvest")
install.packages("stringr")
install.packages("xlsx")
install.packages("dplyr")
library(rvest)
library(stringr)
library(xlsx)
library(dplyr)
getwd()
setwd("C:/Users/kimhs/OneDrive/바탕 화면/movie/moviedata")

#사이트 주소 붙이기
url_vec <- NULL
url <- "http://www.kobis.or.kr/kobis/business/stat/boxs/findDailyBoxOfficeList.do?loadEnd=0&searchType=excel&sSearchFrom="
s <- as.Date("2009-01-01")
e <- as.Date("2018-07-31")
date <- seq(from=s, to=e, by=1)
date <- as.character(date)
ch <- "&sSearchTo="

#사이트 주소 정리
for (i in 1:length(date)) {
  url1 <- paste0(url, date[i], ch, date[i])
  url_vec <- append(url_vec, url1)
}
url_df <- as.data.frame(url_vec)
url_df

##사이트 크롤링 (다운)
for (j in 1:length(date)) {
  b <- str_sub(as.character(url_df[j,1]), start=135, end=144)
  b <- paste0(b,".xls")
  a <- as.character(url_df[j,1])
  download.file(a, destfile = b, method='libcurl')
}

day_df <- NULL

i = 1
for (i in 1:length(date)) {
  #다운 받은 파일 정리 (개봉일이 NA값인것들 처리 등등)
  read_date <- paste0(date[i], ".xlsx")
  day_list <- read.xlsx(read_date, sheetIndex = 1, encoding = 'UTF-8', rowIndex = 4:107, colClasses = "character")
  movie_data <- day_list[-1,]
  movie_data[3]
  head(movie_data)
  exc <- which(is.na(movie_data[3]))
  movie_data <- movie_data[-exc,]
  
  # 필요한 설명변수만 모음
  vec <- c(2,3,9,12,13,15,18,19,20,21,22)
  
  movie_data <- as.matrix(movie_data[,vec])
  movie_data <- gsub(" ", "", movie_data)
  movie_data <- as.data.frame(movie_data)
  colnames(movie_data) <- c("title", "release", "day_audience", "total_audience", "screen", "Nationality",
                            "distributor", "grade", "genre", "director", "actor")
  # date(날짜) 추가
  movie_data$date <- date[i]
  
  # day(개봉일로부터 얼마나 됬는지 처리)
  exc1 <- NULL
  rel <- as.Date(movie_data$release)
  dat <- as.Date(movie_data$date)
  da <- as.Date(as.character((date[i]))
  exc1 <- append(exc1, which(rel-da < 0))
  d <- dat-rel + 1
  
  exc1 <- append(exc1, which(d > 40 | d < 1))  ## d < 1은 시사회...
  d <- d[-exc1]
  movie_data <- movie_data[-exc1,] # exc1을 데이터에서 빼줌
  movie_data$day <- as.integer(d) # 새로운 변수 추가 : 개봉일 - 날짜
  
  # date를 바탕으로 요일 정하기
  
  movie_data$weekday <- weekdays(as.Date(movie_data$date)) ## weekdays함수가 요일을 가져와준다
  
  #휴일 정리 파일 읽어와서 휴일인지 아닌지 정리
  
  holiday2 <- read.csv("holiday_cal.csv")
  holiday <- NULL
  for (x in 1:length(movie_data$date)) {
    r <- which(as.Date(movie_data$date[x]) == as.Date(as.matrix(holiday2[1])))
    holiday <- append(holiday, holiday2[r,2])
    holiday
  }
  movie_data$holiday <- holiday
  head(movie_data)
  
  #날마다 정리된 데이터를 붙인다.
  day_df <- rbind(day_df, movie_data)
  print(i) ## debug, 오류를 해찾기 위해
}

# 붙인 데이터를 title별로 나열한다.
ex <- day_df[order(day_df$title),]

# : 문자 제거
ex <- gsub(':', '', as.matrix(ex)) # 특수문자 제거
ex <- as.data.frame(ex)
write.csv(day_df, file="movie.csv", row.names = F)  # 정렬되지 않은 데이터
write.csv(ex, file="ex.csv", row.names = F)   #정렬된 데이터

real_data <- read.csv("ex.csv", header=T)  #다시 정렬된 데이터를 불러온다 (Factor문제 때문)
mov_title <- levels(real_data$title)   # 제목이 어떤 것이 있는지 확인

# 영화 제목별로 따로따로 저장
movie_name <- NULL
release_list <- NULL

setwd("C:/Users/kimhs/OneDrive/바탕 화면/movie/data")

for (j in 1:length(mov_title)) {
  mov_title[j] <- gsub("/|:", "" , mov_title[j])
  wh <- which(mov_title[j] == real_data$title)
  sub_df <- real_data[wh,]
  sub_df[1,3] <- sub_df[1,4] # 시사회, 첫번째 데이터만 바꾸면 된다
  ti <- paste0(mov_title[j], ".csv")
  if (is.na(sub_df[35,1]) == F && sub_df[nrow(sub_df),4] >50000) {
    write.csv(sub_df, ti, row.names = F)
    movie_name <- append(movie_name,mov_title[j])
    sub_df[.2] <- as.character(sub_df[,2])
    release_list <- append(release_list,sub_df[1,2])
  }
}

total_df <- NULL
last_df <- NULL
real_data <- read.csv("ex.csv", header=T)  #다시 정렬된 데이터를 불러온다 (Factor문제 때문)
mov_title <- levels(real_data$title)   # 제목이 어떤 것이 있는지 확인
# 영화 제목별로 따로따로 저장
#setwd("C:/Users/user/Desktop/worked_data")
for (j in 1:length(mov_title)) {
  wh <- which(mov_title[j] == real_data$title)
  sub_df <- real_data[wh,]
  sub_df[1,3] <- sub_df[1,4]
  ti <- paste0(mov_title[j], ".csv")
  if (is.na(sub_df[35,1]) == F && sub_df[nrow(sub_df),4] >50000) {
    total_df <- rbind(total_df, sub_df)
    last_df <- rbind(last_df, sub_df[nrow(sub_df),])
  }
}
write.csv(total_df, "전체영화모.csv", row.names = F)
write.csv(last_df, "영화누적관객수.csv", row.names = F)


## 관객수가 10만 이하