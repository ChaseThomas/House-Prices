#Change accordingly
setwd("~/desktop/CDS/house")

library(readr)
library(randomForest)
library(caret)
library(corrplot)
library(gridExtra)
library(ggplot2)
library(lubridate)

train <- read.csv("train.csv", stringsAsFactors=TRUE)
test  <- read.csv("test.csv",  stringsAsFactors=TRUE)

sapply(train, function(x)any(is.na(x)))
#Alley, PoolQC, Fence and MiscFeature have WAY more nulls than the other variables (>1000), so remove them
train<- train[,-c(7,73,74,75)]
#---------------------------
test <- test[,-c(7,73,74,75)]
#---------------------------

# Get rid of columns with near zero variance
nzv <- nearZeroVar(train, saveMetrics= TRUE)
badCols <- nearZeroVar(train)
train_variance <- train[, -badCols]
#---------------------------
test_variance <- test[, -badCols]
#---------------------------

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
numerical_test <- extractNumeric(test)

# delete columns with na values
#numerical_train <- sapply(numerical_train[, colSums(is.na(numerical_train)) > 0], function(col) {
#  col[is.na(col)] <- median(col, na.rm = TRUE)
#});
#str(numerical_train);

#---------------------------
for(i in 1:ncol(numerical_train)){
  numerical_train[is.na(numerical_train[,i]), i] <- median(numerical_train[,i], na.rm = TRUE)
}
#---------------------------

#numerical_test <- sapply(numerical_test[, colSums(is.na(numerical_test)) > 0], function(col) {
#  col[is.na(col)] <- median(col, na.rm = TRUE)
#});
#str(numerical_test);

#---------------------------
for(i in 1:ncol(numerical_test)){
  numerical_test[is.na(numerical_test[,i]), i] <- median(numerical_test[,i], na.rm = TRUE)
}
#---------------------------

#[is.na(x)] <- median(numerical_train$Fare, na.rm = TRUE)
#nonnan_numerical <- numerical_train[ , colSums(is.na(numerical_train)) == 0]

#M <- cor(nonnan_numerical)
#corrplot(M, tl.cex = .3)

# feature engineering: YrSold and MoSold
numerical_train$MonthAge = (lubridate::year(Sys.Date()) - train$YrSold) * 12 + (lubridate::month(Sys.Date()) - train$MoSold)
numerical_test$MonthAge  = (lubridate::year(Sys.Date()) - test$YrSold)  * 12 + (lubridate::month(Sys.Date()) - test$MoSold)
str(train)

#randomForest Model
rf <- randomForest(SalePrice ~ OverallQual + GrLivArea + TotalBsmtSF
                   + GarageCars + X2ndFlrSF + X1stFlrSF + TotRmsAbvGrd
                   + BsmtFinSF1 + LotArea + MonthAge, data=numerical_train)

# Predict using the test set (code adapted from public Kaggle script in forums and Leo's example)
prediction <- predict(rf, numerical_test)

solution <- data.frame(id = test$Id, SalePrice = prediction)
write.csv(solution, "house_prices_output.csv", row.names = FALSE)


#function to calculate log error
RMSLE <- function(a, p) {
  if (length(a) != length(p)) stop("Actual and Predicted need to be equal lengths!")
  x <- !is.na(a)
  sqrt(sum((log(p[x]+1) - log(a[x]+1))^2)/sum(x))
}

#Create 5 equally size folds
num_folds = 5
folds <- cut(seq(1,nrow(train)),breaks=num_folds,labels=FALSE)

for(i in 1:num_folds){
  #Segement your data by fold using the which() function
  testIndexes <- which(folds==i,arr.ind=TRUE)
  trainData <- numerical_train[testIndexes, ]
  testData <- numerical_train[-testIndexes, ]
  train_y = trainData
  test_y = testData$SalePrice
  
  rf = randomForest(SalePrice ~ OverallQual + GrLivArea + TotalBsmtSF
                     + GarageCars + X2ndFlrSF + X1stFlrSF + TotRmsAbvGrd
                     + BsmtFinSF1 + LotArea + MonthAge,train_y, ntree=100, importance=TRUE)
  pred = predict(rf, testData)
  print(RMSLE(pred, test_y))
}