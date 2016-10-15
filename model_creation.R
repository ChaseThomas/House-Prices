library(readr)
library(randomForest)
library(caret)


train <- read.csv("train.csv", stringsAsFactors=TRUE)
test  <- read.csv("test.csv",  stringsAsFactors=TRUE)

#Alley, PoolQC, Fence and MiscFeature have WAY more nulls than the other variables (>1000), so remove them
train<- train[,-c(7,73,74,75)]

#GTFO NAs
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- 0
train$MasVnrArea[is.na(train$MasVnrArea)] <- 0
train$LotFrontage[is.na(train$LotFrontage)] <- 0

#Interactions based on correlation
train$year_qual <- train$YearBuilt*train$OverallQual #overall condition
train$year_r_qual <- train$YearRemodAdd*train$OverallQual #quality x remodel
train$qual_bsmt <- train$OverallQual*train$TotalBsmtSF #quality x basement size


train$livarea_qual <- train$OverallQual*train$GrLivArea #quality x living area
train$qual_bath <- train$OverallQual*train$FullBath #quality x baths

#quick fix till we find a better way to fix missing values, which will vastly improve prediciton accuracy
train <- na.omit(train)
test <- na.omit(test)

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