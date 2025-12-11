library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(caret)
library(cv)

## set working directory
setwd("~/GitHub/data-analytics-leer10/lab06/")

## read NY Housing Data
NY_Housing_data <- read_csv("NY-House-Dataset.csv")

## Clean data and set up train/test data split
nyhousing <- na.omit(NY_Housing_data)

# clean up outliers
Q1 <- quantile(nyhousing$PRICE, 0.25, na.rm = TRUE)
Q3 <- quantile(nyhousing$PRICE, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

Q1.1 <- quantile(nyhousing$PROPERTYSQFT, 0.25, na.rm = TRUE)
Q3.1 <- quantile(nyhousing$PROPERTYSQFT, 0.75, na.rm = TRUE)
IQR.1 <- Q3.1 - Q1.1

nyhousing <- subset(nyhousing, PRICE > (Q1 - 1.5*IQR) & PRICE < (Q3 + 1.5*IQR))
nyhousing <- subset(nyhousing, PROPERTYSQFT > (Q1.1 - 1.5*IQR.1) & PROPERTYSQFT < (Q3.1 + 1.5*IQR.1))

# plot housing data
ggplot(nyhousing, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

train.indexes <- sample(nrow(nyhousing), 0.75*nrow(nyhousing))
train <- nyhousing[train.indexes, ]
test <- nyhousing[-train.indexes, ]


## Model 1: Linear Regression Model ##
lin.mod1 <- lm(PRICE~PROPERTYSQFT, train)

summary(lin.mod1)

# Evaluation
cv.mod1 <- cv(lin.mod1)
summary(cv.mod1)

lm.pred1 <- predict(lin.mod1, test)

## err = predicted - real
err <- lm.pred1-test$PRICE

## MAE
abs.err <- abs(err)
mean.abs.err <- mean(abs.err)
mean.abs.err

## MSE
sq.err <- err^2
mean.sq.err <- mean(sq.err)
mean.sq.err

## RMSE
sq.err <- err^2
mean.sq.err <- mean(sq.err)
root.mean.sq.err <- sqrt(mean.sq.err)
root.mean.sq.err

## Model 2: Support Vector Regression

k = 100
mae1 <- c()
mse1 <- c()
rmse1 <- c()

for (i in 1:k) {
  train.indexes <- sample(nrow(nyhousing),0.75*nrow(nyhousing))
  
  train <- nyhousing[train.indexes,]
  test <- nyhousing[-train.indexes,]
  
  svm.mod <- svm(log10(PRICE) ~ log10(PROPERTYSQFT), train, kernel="radial")
  
  svm.pred <- predict(svm.mod, test)  
  
  err <- svm.pred-log10(test$PRICE)
  
  abs.err <- abs(err)
  mean.abs.err <- mean(abs.err)
  
  sq.err <- err^2
  mean.sq.err <- mean(sq.err)
  
  root.mean.sq.err <- sqrt(mean.sq.err)  
  
  mae1 <- c(mae1,mean.abs.err)
  mse1 <- c(mse1,mean.sq.err)
  rmse1 <- c(rmse1,root.mean.sq.err)
}

mean(mae1)
mean(mse1)
mean(rmse1)

results1 <- data.frame(mae=mean(mae1), mse=mean(mse1), rmse=mean(rmse1))
results1

## Model 3: Random Forest Regression Model ##

train.indexes <- sample(nrow(nyhousing),0.75*nrow(nyhousing))

train <- nyhousing[train.indexes,]
test <- nyhousing[-train.indexes,]

rf.mod <- train(PRICE~PROPERTYSQFT, data=train, method="rf")
rf.pred <- predict(rf.mod, test)

## err = predicted - real
err <- rf.pred-test$PRICE

## MAE
abs.err <- abs(err)
mean.abs.err <- mean(abs.err)
mean.abs.err

## MSE
sq.err <- err^2
mean.sq.err <- mean(sq.err)
mean.sq.err

## RMSE
sq.err <- err^2
mean.sq.err <- mean(sq.err)
root.mean.sq.err <- sqrt(mean.sq.err)
root.mean.sq.err
