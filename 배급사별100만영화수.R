rm(list=ls())

library(dplyr)
library(stringr)
library(tidyr)

setwd("C:/Users/kimhs/OneDrive/바탕 화면/movie/data")
movie <- read.csv("total_data.csv", header = T, stringsAsFactors = F)
#movie <- movie %>% filter(total_audience > 1000000)
agency <- movie$distributor
agency <- gsub(" ", "" ,agency)
agency <- as.data.frame(agency)

movie %>% filter(title == "기억의밤") %>% select(distributor)

ex <- agency[grepl(",", agency[,]) == TRUE,]
na <- movie[movie$distributor == "", 1]

ex_idx = NULL
i = 1
j = 1
k = 1
for(i in 1:nrow(agency)) {
  if(ex[k] == agency[i,]) {
    print(i)
    k = k+1
    if(k > length(ex)) { k =1 }
    ex_idx = append(ex_idx, i)
  }
  else if(movie[i,1] == na[j]) {
    print(i)
    j = j+1
    if(j > length(na)) { j = 1}
    ex_idx = append(ex_idx, i)
  }
}


movie[ex_idx,"title"]

dis2 <- movie$distributor[ex_idx]
dis2 <- dis2[-c(10,40,63,72)]
dis2 <- as.data.frame(dis2, stringsAsFactors =F)
dis3 <- as.data.frame(dis2[21,], stringsAsFactors = F)
dis2 <- as.data.frame(dis2[-21,], stringAsFactors = F)
names(dis2) <- "dis2"
names(dis3) <- "dis3"

a <- str_split(agency[grepl(",", agency[,]) == TRUE,], ",")
a <- unlist(a)

agency <- agency[-ex_idx,]
agency <- c(agency,a)
ag <- unique(agency)

dis <- as.data.frame(agency)

j = 1 
dis_n <- NULL
for(j in 1:NROW(ag)){
    n <- nrow(dis %>% filter(ag[j] == dis))
    print(c(j, n))
    dis_n <- append(dis_n, n)
  }  

disdf <- data.frame(ag,dis_n, stringsAsFactors = F)
disdf <- as.data.frame(disdf, stringAsFactors=F)
sum(dis_n)
table(dis_n)


dis2 <- as.data.frame(dis2)
dis2 <- separate(dis2, dis2, c("1","2"), sep = ",")

p = 1
for(p in 1:nrow(dis2)){
  x2 <- disdf %>% filter(dis2[p,1] == ag)
  y2 <- disdf %>% filter(dis2[p,2] == ag)
  if(as.numeric(x2[2]) < as.numeric(y2[2])) {d <- as.character(y2[1,1])} else{d <- as.character(x2[1,1])}
  print(d)
  movie$distributor[ex_idx[p]] <- d 
} 


dis3 <- separate(dis3,dis3, c("1","2","3"), sep = ",")
q = 1
for(q in 1:nrow(dis3)){
  x3 <- disdf %>% filter(dis3[q,1] == ag)
  y3 <- disdf %>% filter(dis3[q,2] == ag)
  z3 <- disdf %>% filter(dis3[q,3] == ag)
  maxi <- 1
  if(as.numeric(x3[2]) >= maxi) {maxi <- as.numeric(x3[2]); d <- as.character(x3[1,1])} 
  if(as.numeric(y3[2]) >= maxi) {maxi <- as.numeric(y3[2]); d <- as.character(y3[1,1])}
  if(as.numeric(z3[2]) >= maxi) {d <- as.character(z3[1,1])}
  print(d)
  movie$distributor[ex_idx[q]] <- d 
} 



c <- read.csv("score(1).csv",header=T,stringsAsFactors = F)
c$distributor
c$distributor <- gsub(" " , "" , c$distributor)


L1 <- as.data.frame(c %>% filter(dis_score == 1) %>% select(distributor), stringsAsFactors =F)
L2 <- as.data.frame(c %>% filter(dis_score == 2) %>% select(distributor), stringsAsFactors =F)
L3 <- as.data.frame(c %>% filter(dis_score == 3) %>% select(distributor), stringsAsFactors =F)
L4 <- as.data.frame(c %>% filter(dis_score == 4) %>% select(distributor), stringsAsFactors =F)

L1 <- unique(L1)
L2 <- unique(L2)
L3 <- unique(L3)
L4 <- unique(L4)



l1 <- 1
K1 <- NULL
for (l1 in 1:nrow(L1)) {
  a <- ifelse(movie$distributor == L1[l1,1], 1,0)
  k <-  which(a == 1)
  print(k)
  K1 <- append(K1,k)
}

l2 <- 1
K2 <- NULL
for (l2 in 1:nrow(L2)) {
  a <- ifelse(movie$distributor == L2[l2,1], 1,0)
  k <-  which(a == 1)
  print(k)
  K2 <- append(K2,k)
}


l3 <- 1
K3 <- NULL
for (l3 in 1:nrow(L3)) {
  a <- ifelse(movie$distributor == L3[l3,1], 1,0)
  k <-  which(a == 1)
  print(k)
  K3 <- append(K3,k)
}

l4 <- 1
K4 <- NULL
for (l4 in 1:nrow(L4)) {
  a <- ifelse(movie$distributor == L4[l4,1], 1,0)
  k <-  which(a == 1)
  print(k)
  K4 <- append(K4,k)
}

length(K1)
length(K2)
length(K3)
length(K4)



a <- NULL
a[K1] <- 1
a[K2] <- 2
a[K3] <- 3
a[K4] <- 4
a[-c(K1,K2,K3,K4)] <- 0

movie$a <- a

write.csv(movie,"a.csv",row.names = F)
