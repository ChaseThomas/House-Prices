# Scoring
# Contains functions to help us evaluate our models.

library(caret)

# Function to calculate log error
RMSLE <- function(a, p) {
  if (length(a) != length(p)) stop("Actual and Predicted need to be equal lengths!")
  x <- !is.na(a)
  sqrt(sum((log(p[x]+1) - log(a[x]+1))^2)/sum(x))
}

crossValidate <- function(train, features) {
  # Arguments:
  # train - the training data set
  # features - the features we want to look at (must be the same as in the model!)

  # Create 5 equally sized folds
  num_folds = 5
  folds <- cut(seq(1,nrow(train)), breaks = num_folds, labels = FALSE)

  for(i in 1:num_folds){
    # Segment data by fold using the which() function
    testIndexes <- which(folds==i,arr.ind=TRUE)
    trainData <- numerical_train[testIndexes, ]
    testData <- numerical_train[-testIndexes, ]
    test_y <- testData$SalePrice

    rf <- randomForest(
      trainData[features],
      trainData$SalePrice,
      ntree = 500,
      importance = TRUE)
    pred <- predict(rf, testData)
    print(RMSLE(pred, test_y))
  }
}
