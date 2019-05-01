library(stringr)
file_names <- list.files("C:\\Users\\Administrator\\Desktop\\빅콘테스트\\NEW") #file list 
head(file_names)

i<-1 # csv 빼기
new_file_names <- NULL
for (i in 1:length(file_names)){
  #charactor의 개수
  nchar(file_names[i])
  #file_name
  name <- substr(file_names[i], 0, nchar(file_names[i])-4)#처음부터 전체 갯수에서 네개를 빼겠다  
  new_file_names <- append(new_file_names, name) # newfilenames에 붙이자
}

new_file_names
file_names# csv가 붙어있는 파일 

setwd("C:/Users/user/Desktop/movie2")
i <-1
total_data <- NULL
for (i in 1:length(file_names)){
  file <- read.csv(file_names[i], header = T, stringsAsFactors = F) #파일 열기
  file18 <- file[18,] #18번째 row만 가져오기 
  file18[1,5] <- file[1,5]# 첫날 스크린수가 결정적, 첫날의스크린수를 피처로 사용하고 싶음
  total_data <- rbind(total_data, file18) 
  print(i)
}
total_data

setwd("C:/Users/user/Desktop")
write.csv(total_data, "total_data18.csv", row.names = F)
