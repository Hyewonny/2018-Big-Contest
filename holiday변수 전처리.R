~~~~holiday~~~~ (양력 -> 음력 변환은 엑셀에서 함)
getwd()
setwd("c:/Users/박보정/Desktop")
movie40 <- read.csv("total40_real_final.csv",header=1)

#lunar_day 변수를 숫자형으로 만든다.
movie40
class(movie40$lunar_day)
movie40$lunar_day <- sub('-',"",movie40$lunar_day)
movie40$lunar_day <- substr(movie40$lunar_day,1,4)
movie40$lunar_day <- as.integer(movie40$lunar_day)

#lunar_day가 명절을 포함하면 holiday에 1, 아니면 0을 넣는다.

movie40$holiday1 <- ifelse(movie40$lunar_day > 705 & movie40$lunar_day < 817, 1, 0)
movie40$holiday2 <- ifelse(movie40$lunar_day > 1121 | movie40$lunar_day < 103, 1, 0)
movie40$holiday <- movie40$holiday1 + movie40$holiday2

library(dplyr)
movie40 %>% filter(holiday == 1) %>% select(release, lunar_day)

new_movie40 <- movie40%>% select(-c(year, month, day, lunar_day, holiday1, holiday2))

write.csv(new_movie40, "newholiday40.csv")

##18일도 똑같이
movie18 <- read.csv("total_data18.csv",header=1)

#lunar_day 변수를 숫자형으로 만든다.
movie18
class(movie18$lunar_day)
movie18$lunar_day <- sub('-',"",movie18$lunar_day)
movie18$lunar_day <- substr(movie18$lunar_day,1,4)
movie18$lunar_day <- as.integer(movie18$lunar_day)

#lunar_day가 명절을 포함하면 holiday에 1, 아니면 0을 넣는다.

movie18$holiday1 <- ifelse(movie18$lunar_day > 727 & movie18$lunar_day < 817, 1, 0)
movie18$holiday2 <- ifelse(movie18$lunar_day > 1213 | movie18$lunar_day < 103, 1, 0)
movie18$holiday <- movie18$holiday1 + movie18$holiday2

movie18 %>% filter(holiday == 1) %>% select(release, lunar_day)

new_movie18 <- movie18%>% select(-c(year, month, day, lunar_day, holiday1, holiday2))

write.csv(new_movie18, "holiday18.csv")