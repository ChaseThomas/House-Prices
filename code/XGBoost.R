# Random Forest
# Making predictions using the XGradientBoost model.

# This file must be run in the /data directory to work!
# Run the following command before executing:
# setwd("~/<insert your directory here>/House-Prices/data")

library(xgboost)
library(caret)
source("../code/DataProcessing.R")
source("../code/Scoring.R")

rounds <- 10

train <- read.csv("../data/train.csv", stringsAsFactors = TRUE)
test  <- read.csv("../data/test.csv",  stringsAsFactors = TRUE)
features <- c("OverallQual", "GrLivArea", "TotalBsmtSF",
             "GarageCars", "X2ndFlrSF", "X1stFlrSF", "TotRmsAbvGrd",
             "BsmtFinSF1", "LotArea", "MonthAge")

train_test <- processTrainTest(train, test, features)

# processTrainTest returns a list of four vectors:
# processed_train: the training set with selected features only
# processed_test: the testing set with selected features only
# train_y: the y-value (thing we are trying to predict) for the training set
# test_y: the y-value (thing we are trying to predict) for the testing set

processed_train <- data.frame(train_test[1])
processed_test <- data.frame(train_test[2])
train_y <- unlist(train_test[3])
test_y <- unlist(train_test[4])

applyXGBoost <- function(train, train_y) {
  xgboost(
    data = as.matrix(train),
    label = train_y,
    nround = rounds,
    objective = "reg:linear"
  )
}

applyPrediction <- function(model, test) {
  predict(model, as.matrix(test))
}

# Random forest
xg <- applyXGBoost(processed_train, train_y)
# Predict using the test set (code adapted from public Kaggle script in forums and Leo's example)
prediction <- applyPrediction(xg, processed_test)

solution <- data.frame(id = test$Id, SalePrice = prediction)
write.csv(solution, "house_prices_output.csv", row.names = FALSE)

crossValidate(train, features, applyXGBoost, applyPrediction)
