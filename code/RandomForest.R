# Random Forest
# Making predictions using the Random Forest model.

# This file must be run in the /data directory to work!
# Run the following command before executing:
# setwd("~/<insert your directory here>/House-Prices/data")

library(randomForest)
library(caret)
source("../code/DataProcessing.R")
source("../code/Scoring.R")

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

crossValidate(train, features)
