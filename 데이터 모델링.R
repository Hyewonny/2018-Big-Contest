getwd()
setwd("C:/Users/user/Desktop")

library(MASS)

library(caret)

library(randomForest)
## function for MAPE


## function for RMSE
RMSE <- function(target,pred){
  error = (target-pred)^2
  return(sqrt(mean(error)))
}

a <- read.csv("40days.csv" , sep =",",stringsAsFactors = F)

library(data.table)

a <- a[,-c(2,3,9:17)]

for (i in 1:nrow(a)) {
        a$expect <- gsub(",", "",a$expect)}
a$expect <- as.numeric(a$expect)
a[336,10]  <- 0
str(a)

a<- read.csv("18days.csv", sep =",",stringsAsFactors = F)
a <- a[,-c(2,3,5,7,8,9,10,11,12,13,14,15,16)]
for (i in 1:nrow(a)) {
  a$expect <- gsub(",", "",a$expect)}
a$expect <- as.numeric(a$expect)
a$release_audience <- as.numeric(a$release_audience)


a[is.na(a$release_audience)]<- 0
str(a)
## make folds for cross validation




control = trainControl(method='cv', search='grid', number=10)
idx = createDataPartition(a$total_audience, p=.8, list=F)
idx

data.train = a[idx, ]
data.train <- data.train[,-1]
data.test = a[-idx, ]

data.test <- data.test[,-1]




lin.model <- lm(total_audience~., data=data.train, na.action=na.exclude)
lin.model <- lm(log(total_audience)~., data=data.train, na.action=na.exclude)

plot(lin.model)

lin.model = train(
  log(total_audience) ~ .,
  data = data.train,
  na.action=na.exclude,
  trControl = control,
  method = 'lm')


pred = predict(lin.model,data.test)
lin_pred = exp(pred)

RMSE(data.test$total_audience,lin_pred)

rf.model = train(
   total_audience~ .,
  data = data.train,
  tuneGrid = data.frame(.mtry=c(4, 6, 8)),
  trControl = control,
  na.action=na.exclude,
  method = 'rf')

pred = predict(rf.model,data.test)
RMSE(data.test$total_audience,pred)

xgb.grid = expand.grid(
  .nrounds = 100,
  .eta = c(0.1, 0.3, 0.5),
  .gamma = 1,
  .max_depth = c(3, 5),
  .min_child_weight = 1,
  .colsample_bytree = 1,
  .subsample=1
)

data.train <- data.train[-7119,]
xgb.model <- train(
  total_audience ~ .,
  data = data.train,
  tuneGrid = xgb.grid,
  trControl = control,
  na.action=na.exclude,
  method = 'xgbTree'
)
pred = predict(xgb.model,data.test)
xgb.pred = predict(xgb.model,data.test)

RMSE(data.test$total_audience,xgb.pred)



