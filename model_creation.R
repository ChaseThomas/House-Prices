#Change accordingly
setwd("~/datasci/House-Prices")

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
numerical_test <- extractNumeric(test)

# delete columns with na values
numerical_train <- sapply(numerical_train[, colSums(is.na(numerical_train)) > 0], function(col) {
  col[is.na(col)] <- median(col, na.rm = TRUE)
});
str(numerical_train);

numerical_test <- sapply(numerical_test[, colSums(is.na(numerical_test)) > 0], function(col) {
  col[is.na(col)] <- median(col, na.rm = TRUE)
});
str(numerical_test);

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
