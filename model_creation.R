install.packages("corrplot")

library(readr)
library(randomForest)
library(caret)
library(corrplot)
library(gridExtra)
library(ggplot2)
library(lubridate)

#Change accordingly
setwd("~/desktop/CDS/house")

train <- read.csv("train.csv", stringsAsFactors=TRUE)
test  <- read.csv("test.csv",  stringsAsFactors=TRUE)

sapply(train, function(x)any(is.na(x))) 
#Alley, PoolQC, Fence and MiscFeature have WAY more nulls than the other variables (>1000), so remove them
train<- train[,-c(7,73,74,75)]

# Get rid of columns with near zero variance
nzv <- nearZeroVar(train, saveMetrics= TRUE)
badCols <- nearZeroVar(train)
train_variance <- train[, -badCols]

# helper function 
extractNumeric <- function(data) {
  factor_cols <- names(Filter(function(x) x=="factor", sapply(data, class)))
  for (col in factor_cols) {
    data[,col] <- ordered(data[,col])
    data[,col] <- as.numeric(data[,col])
  }
  return(data)
}

numerical_train <- extractNumeric(train)

# delete columns with na values
nonnan_numerical <- numerical_train[ , colSums(is.na(numerical_train)) == 0]

M <- cor(nonnan_numerical)
corrplot(M, tl.cex = .3)

# feature engineering: YrSold and MoSold
train$MonthAge = (lubridate::year(Sys.Date()) - train$YrSold) * 12 + (lubridate::month(Sys.Date()) - train$MoSold)
test$MonthAge  = (lubridate::year(Sys.Date()) - test$YrSold)  * 12 + (lubridate::month(Sys.Date()) - test$MoSold)
str(train)

#partition that data
partition <- createDataPartition(y=train$SalePrice,
                                 p=.8,
                                 list=F)
training <- train[partition,]
testing <- train[-partition,]

#randomForest Model
rf <- randomForest(SalePrice ~ ., data=training)

# Predict using the test set (code adapted from public Kaggle script in forums and Leo's example)
prediction <- predict(rf, testing)

solution <- data.frame(HouseID = test$Id, Price = prediction)

