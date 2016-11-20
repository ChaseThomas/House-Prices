# Data Processing
# Processes raw data frames and returns objects we can use our models on.

library(readr)
library(lubridate)

processTrainTest <- function(train, test, features) {
  # Arguments:
  # train: the raw training data frame
  # test: the raw testing data frame
  # features: the features we're looking at
  #
  # Returns a vector of four items:
  # processed_train: the training set with selected features only
  # processed_test: the testing set with selected features only
  # train_y: the y-value (thing we are trying to predict) for the training set
  # test_y: the y-value (thing we are trying to predict) for the testing set

  # Get rid of columns with near zero variance
  nzv <- nearZeroVar(train, saveMetrics= TRUE)
  badCols <- nearZeroVar(train)
  train_variance <- train[, -badCols]
  test_variance <- test[, -badCols]

  # Helper function for turning factors into numeric columns.
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

  for(i in 1:ncol(numerical_train)){
    numerical_train[is.na(numerical_train[,i]), i] <- median(numerical_train[,i], na.rm = TRUE)
  }

  for(i in 1:ncol(numerical_test)){
    numerical_test[is.na(numerical_test[,i]), i] <- median(numerical_test[,i], na.rm = TRUE)
  }

  # Feature engineering: YrSold and MoSold

  numerical_train$MonthAge <-
    (lubridate::year(Sys.Date()) - train$YrSold) * 12 +
    (lubridate::month(Sys.Date()) - train$MoSold)
  numerical_test$MonthAge  <-
    (lubridate::year(Sys.Date()) - test$YrSold)  * 12 +
    (lubridate::month(Sys.Date()) - test$MoSold)

  index <- createDataPartition(train$Id, p = .8, list = FALSE, times = 1)
  df_train <- numerical_train[index, ]
  df_train$Id <- NULL
  train_y <- df_train$SalePrice
  df_test <- numerical_train[-index, ]
  df_test$Id <- NULL
  test_y <- df_test$SalePrice

  return(list(df_train[features], numerical_test[features], train_y, test_y))
}
