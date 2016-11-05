# Random Forest
# Making predictions using the Random Forest model.

library(readr)
library(randomForest)
library(caret)
library(corrplot)
library(gridExtra)
library(ggplot2)
library(lubridate)
source("../code/DataProcessing.R")

numTrees <- 100

train <- read.csv("train.csv", stringsAsFactors = TRUE)
test  <- read.csv("test.csv",  stringsAsFactors = TRUE)
features = c("OverallQual", "GrLivArea", "TotalBsmtSF",
            "GarageCars", "X2ndFlrSF", "X1stFlrSF", "TotRmsAbvGrd",
            "BsmtFinSF1", "LotArea", "MonthAge")

train_test <- processTrainTest(train, test, features)
processed_train <- data.frame(train_test[1])
processed_test <- data.frame(train_test[2])
train_y <- unlist(train_test[3])
test_y <- unlist(train_test[4])

# Random forest
rf <- randomForest(processed_train, train_y, ntree=numTrees, importance=TRUE)
# Predict using the test set (code adapted from public Kaggle script in forums and Leo's example)
prediction <- predict(rf, processed_test)

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
folds <- cut(seq(1,nrow(train)), breaks = num_folds, labels = FALSE)

for(i in 1:num_folds){
  #Segement your data by fold using the which() function
  testIndexes <- which(folds==i,arr.ind=TRUE)
  trainData <- numerical_train[testIndexes, ]
  testData <- numerical_train[-testIndexes, ]
  test_y <- testData$SalePrice

  rf <- randomForest(trainData[features],trainData$SalePrice, ntree=500, importance=TRUE)
  pred <- predict(rf, testData)
  print(RMSLE(pred, test_y))
}
