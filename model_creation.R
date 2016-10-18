library(readr)
library(randomForest)
library(caret)

library(corrplot)
library(gridExtra)
library(ggplot2)

train <- read.csv("train.csv", stringsAsFactors=TRUE)
test  <- read.csv("test.csv",  stringsAsFactors=TRUE)

#Alley, PoolQC, Fence and MiscFeature have WAY more nulls than the other variables (>1000), so remove them
train<- train[,-c(7,73,74,75)]

nzv <- nearZeroVar(train, saveMetrics= TRUE)
badCols <- nearZeroVar(train)
train <- train[, -badCols]

numerical_train <- extractNumeric(train[,!(names(train) %in% col_null)])
M <- cor(numerical_train)
corrplot(M, tl.cex = .3)

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