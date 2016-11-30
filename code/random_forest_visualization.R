setwd("/Users/jaredlim/Desktop/Programming/Kaggle/House-Prices/Data/")
test <- read.csv("test.csv")
train <- read.csv("cleaned_train.csv")

library(randomForest)
library(dplyr)
library(ggplot2)

set.seed(42)

train <- train[,-c(1,2)] # exclude row numbers and Id

rf <- randomForest(SalePrice ~ ., data=train)

set <- data_frame(feature=setdiff(colnames(train), "SalePrice"),
                             importance=as.vector(importance(rf)))
set <- arrange(set, desc(importance))
set$feature <- factor(set$feature, levels=set$feature)

p <- ggplot(set, aes(x=feature, weight=importance, fill=feature))
p <- p + geom_bar() + ggtitle("Feature by Influence")
p <- p + xlab("House Price Features") + ylab("Feature Importance")
p <- p + scale_fill_discrete(name="House Price Features")
p + theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title=element_text(size=15),
          plot.title=element_text(size=20),
          legend.title=element_text(size=17),
          legend.text=element_text(size=10))

