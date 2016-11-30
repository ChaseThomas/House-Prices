# setwd("/Users/jaredlim/Desktop/Programming/Kaggle/House-Prices/Data/")
test <- read.csv("test.csv")
train <- read.csv("cleaned_train.csv")

# install.packages
library(ggrepel)
library(ggplot2)

options(scipen=999)
hist(train$SalePrice)

ggplot(train, aes(x = SalePrice)) +
  geom_histogram()

ggplot(train,
       aes(x=YrSold,
           y=SalePrice)) +
  geom_point()

# compare YearBuilt and SalePrice
model <- subset(train)

model$pred.SP <- predict(lm(SalePrice ~ YearBuilt, data = model))
ggplot(model,
       aes(x=YearBuilt,
           y=SalePrice)) +
  #geom_point(aes(color=SalePrice)) + geom_line(aes(y = pred.SP), color="red")

geom_point(aes(color=SalePrice, shape = KitchenQual))

# compare YrSold and SalePrice
model <- subset(train)

model$pred.SP <- predict(lm(SalePrice ~ YrSold, data = model))
ggplot(model,
       aes(x=YrSold,
           y=SalePrice)) +
  geom_point(aes(color=SalePrice)) + geom_line(aes(y = pred.SP), color="red")

# MonthAge
model <- subset(train, SalePrice < 400000)

model$MonthAge <-
  (lubridate::year(Sys.Date()) - model$YrSold) * 12 +
  (lubridate::month(Sys.Date()) - model$MoSold)

model$pred.SP <- predict(lm(SalePrice ~ MonthAge, data = model))
ggplot(model,
       aes(x=MonthAge,
           y=SalePrice)) +
  geom_point(aes(color=SalePrice)) + geom_line(aes(y = pred.SP), color="red")






