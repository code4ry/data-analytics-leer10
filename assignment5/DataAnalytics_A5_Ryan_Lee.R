## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(caret)

## load data
setwd("~/GitHub/data-analytics-leer10/assignment5/")

NYC_city_data <- read_csv("NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv")
manhattan<- NYC_city_data[NYC_city_data$BOROUGH == 1, ]

## exploring data patterns and trends, variable distributions
manhattan$SALE.PRICE <- as.numeric(gsub("[^0-9]", "", manhattan$`SALE PRICE`))
manhattan$GROSS.SQUARE.FEET <- as.numeric(gsub("[^0-9]", "", manhattan$`GROSS SQUARE FEET`))
manhattan$LAND.SQUARE.FEET <- as.numeric(gsub("[^0-9]", "", manhattan$`LAND SQUARE FEET`))
manhattan$YEAR.BUILT <- as.numeric(manhattan$`YEAR BUILT`)
manhattan <- manhattan[!is.na(manhattan$SALE.PRICE) & manhattan$SALE.PRICE > 0, ]

## remove outliers
Q1 <- quantile(manhattan$SALE.PRICE, 0.25, na.rm = TRUE)
Q3 <- quantile(manhattan$SALE.PRICE, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

Q1.1 <- quantile(manhattan$YEAR.BUILT, 0.25, na.rm = TRUE)
Q3.1 <- quantile(manhattan$YEAR.BUILT, 0.75, na.rm = TRUE)
IQR.1 <- Q3.1 - Q1.1

manhattan <- subset(manhattan, SALE.PRICE > (Q1 - 1.5*IQR) & SALE.PRICE < (Q3 + 1.5*IQR))
manhattan <- subset(manhattan, YEAR.BUILT > (Q1.1 - 1.5*IQR.1) & YEAR.BUILT < (Q3.1 + 1.5*IQR.1))
manhattan <- subset(manhattan, GROSS.SQUARE.FEET > 200 & GROSS.SQUARE.FEET < quantile(GROSS.SQUARE.FEET, 0.99, na.rm = TRUE))

## summaries
summary(manhattan$SALE.PRICE)
summary(manhattan$YEAR.BUILT)
summary(manhattan$GROSS.SQUARE.FEET)

## plots
ggplot(manhattan, aes(x = SALE.PRICE)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_log10() +
  labs(title = "Distribution of Manhattan Sale Prices", x = "Sale Price (log scale)")

ggplot(manhattan, aes(x = reorder(NEIGHBORHOOD, SALE.PRICE, median), y = SALE.PRICE)) +
  geom_boxplot(fill = "lightblue") +
  coord_flip() +
  scale_y_log10() +
  labs(title = "Median Sale Prices by Neighborhood", x = "Neighborhood", y = "Sale Price (log scale)")

ggplot(manhattan, aes(x = YEAR.BUILT, y = SALE.PRICE)) +
  geom_point(alpha = 0.4, color = "darkred") +
  scale_y_log10() +
  labs(title = "Sale Price vs. Year Built in Manhattan")

## plot cdfs
plot(ecdf(manhattan$SALE.PRICE), do.points=FALSE, verticals=TRUE) 
plot(ecdf(manhattan$YEAR.BUILT), do.points=FALSE, verticals=TRUE) 
plot(ecdf(manhattan$GROSS.SQUARE.FEET), do.points=FALSE, verticals=TRUE) 

## Q-Q plot
qqplot(manhattan$SALE.PRICE, manhattan$`GROSS SQUARE FEET`, xlab = "Q-Q plot for SALE PRICE vs GROSS SQUARE FEET") 
  

### Principal Component Analysis ###
manhattan_data <- manhattan[, sapply(manhattan, is.numeric)]

pairs.panels(manhattan_data,gap = 0,bg = c("red", "yellow", "blue")[manhattan_data$`SALE PRICE`],pch=21)

manhattan_data <- manhattan_data[complete.cases(manhattan_data), ]
manhattan_data <- manhattan_data[apply(manhattan_data, 1, function(x) all(is.finite(x))), ]


manhattan_matrix <- as.matrix(manhattan_data)
manhattan_matrix <- scale(manhattan_matrix, center = TRUE, scale = TRUE)

principal_components <- princomp(manhattan_matrix)

principal_components$loadings

autoplot(principal_components, data = manhattan_data, colour = 'SALE.PRICE',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, scale = 0)

## regression analysis using most contributing features
features <- c("YEAR.BUILT", "SALE.PRICE", "Latitude", "Longitude", "BIN", "BBL", "GROSS.SQUARE.FEET")

manhattan_data <- manhattan[, c(features)]

## accessing multiple regression models for each contributing variable in predicting sale price
for (feature in features){
  formula <- as.formula(paste("SALE.PRICE ~", feature))
  lin.mod <- lm(formula, data = manhattan_data)
  print(feature)
  print(summary(lin.mod))
}

### Classification Models for borough data

manhattan_data <- manhattan[, c(
  "NEIGHBORHOOD",
  "YEAR.BUILT",
  "SALE.PRICE",
  "Latitude",
  "Longitude",
  "GROSS.SQUARE.FEET"
)]

manhattan_data <- na.omit(manhattan_data)
manhattan_data$NEIGHBORHOOD <- factor(manhattan_data$NEIGHBORHOOD)

trainIndex <- createDataPartition(manhattan_data$NEIGHBORHOOD, p = 0.7, list=FALSE)
train_manhattan <- manhattan_data[trainIndex, ]
test_manhattan <- manhattan_data[-trainIndex, ]

train_manhattan <- na.omit(train_manhattan)
test_manhattan <- na.omit(test_manhattan)

# kNN, random forest, and decision tree models
mod.knn <- train(NEIGHBORHOOD~., data=train_manhattan, method="knn")
mod.rf <- train(NEIGHBORHOOD~., data=train_manhattan, method="rf")
mod.rpart <- train(NEIGHBORHOOD~., data=train_manhattan, method="rpart")

# contingency tables and precision/recall/f1 scores
knn.predicted <- predict(mod.knn, test_manhattan)
rf.predicted <- predict(mod.rf, test_manhattan)
rpart.predicted <- predict(mod.rpart, test_manhattan)

cm.knn <- as.matrix(table(Actual = test_manhattan$NEIGHBORHOOD, Predicted = knn.predicted))
cm.rf <- as.matrix(table(Actual = test_manhattan$NEIGHBORHOOD, Predicted = rf.predicted))
cm.rpart <- as.matrix(table(Actual = test_manhattan$NEIGHBORHOOD, Predicted = rpart.predicted))

## evaluate classification models

n = sum(cm.knn) # number of instances
nc = nrow(cm.knn) # number of classes
diag = diag(cm.knn) # number of correctly classified instances per class 
rowsums = apply(cm.knn, 1, sum) # number of instances per class
colsums = apply(cm.knn, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision = diag / colsums
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

cm.knn
data.frame(recall, precision, f1)

n = sum(cm.rf) # number of instances
nc = nrow(cm.rf) # number of classes
diag = diag(cm.rf) # number of correctly classified instances per class 
rowsums = apply(cm.rf, 1, sum) # number of instances per class
colsums = apply(cm.rf, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision = diag / colsums
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(recall, precision, f1)

n = sum(cm.rpart) # number of instances
nc = nrow(cm.rpart) # number of classes
diag = diag(cm.rpart) # number of correctly classified instances per class 
rowsums = apply(cm.rpart, 1, sum) # number of instances per class
colsums = apply(cm.rpart, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision = diag / colsums
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

cm.rpart
data.frame(recall, precision, f1)

####### Question 2 #######

### clean the data

bronx <- NYC_city_data[NYC_city_data$BOROUGH == 2, ]

## exploring data patterns and trends, variable distributions
bronx$SALE.PRICE <- as.numeric(gsub("[^0-9]", "", bronx$`SALE PRICE`))
bronx$GROSS.SQUARE.FEET <- as.numeric(gsub("[^0-9]", "", bronx$`GROSS SQUARE FEET`))
bronx$YEAR.BUILT <- as.numeric(bronx$`YEAR BUILT`)
bronx <- bronx[!is.na(bronx$SALE.PRICE) & bronx$SALE.PRICE > 0, ]

## remove outliers
Q1 <- quantile(bronx$SALE.PRICE, 0.25, na.rm = TRUE)
Q3 <- quantile(bronx$SALE.PRICE, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

Q1.1 <- quantile(bronx$YEAR.BUILT, 0.25, na.rm = TRUE)
Q3.1 <- quantile(bronx$YEAR.BUILT, 0.75, na.rm = TRUE)
IQR.1 <- Q3.1 - Q1.1

bronx <- subset(bronx, SALE.PRICE > (Q1 - 1.5*IQR) & SALE.PRICE < (Q3 + 1.5*IQR))
bronx <- subset(bronx, YEAR.BUILT > (Q1.1 - 1.5*IQR.1) & YEAR.BUILT < (Q3.1 + 1.5*IQR.1))
bronx <- subset(bronx, GROSS.SQUARE.FEET > 200 & GROSS.SQUARE.FEET < quantile(GROSS.SQUARE.FEET, 0.99, na.rm = TRUE))

### Regression Analysis

features <- c("YEAR.BUILT", "SALE.PRICE", "Latitude", "Longitude", "GROSS.SQUARE.FEET")

bronx_data <- bronx[, c(features)]

## accessing multiple regression models for each contributing variable in predicting sale price
for (feature in features) {
  formula <- as.formula(paste("SALE.PRICE ~", feature))
  lin.mod <- lm(formula, data = bronx_data)
  
  print(feature)
  print(summary(lin.mod))
  
  # prepare data for ggplot
  df <- data.frame(
    fitted = lin.mod$fitted.values,
    resid  = lin.mod$residuals
  )
  
  # residual plot
  print(
    ggplot(df, aes(x = fitted, y = resid)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = paste0("Residuals vs Fitted (", feature, ")"))
  )
}


## classification models on bronx data

features <- c("YEAR.BUILT", "SALE.PRICE", "Latitude", "Longitude", "GROSS.SQUARE.FEET", "NEIGHBORHOOD")
bronx_data <- bronx[, c(features)]

bronx_data <- na.omit(bronx_data)
bronx_data$NEIGHBORHOOD <- factor(bronx_data$NEIGHBORHOOD)

trainIndex <- createDataPartition(bronx_data$NEIGHBORHOOD, p = 0.7, list=FALSE)
train_bronx <- bronx_data[trainIndex, ]
test_bronx <- bronx_data[-trainIndex, ]

train_bronx <- na.omit(train_bronx)
test_bronx <- na.omit(test_bronx)

# kNN, random forest, and decision tree models
mod.knn <- train(NEIGHBORHOOD~., data=train_bronx, method="knn")
mod.rf <- train(NEIGHBORHOOD~., data=train_bronx, method="rf")
mod.rpart <- train(NEIGHBORHOOD~., data=train_bronx, method="rpart")

# contingency tables and precision/recall/f1 scores
knn.predicted <- predict(mod.knn, test_bronx)
rf.predicted <- predict(mod.rf, test_bronx)
rpart.predicted <- predict(mod.rpart, test_bronx)

cm.knn <- as.matrix(table(Actual = test_bronx$NEIGHBORHOOD, Predicted = knn.predicted))
cm.rf <- as.matrix(table(Actual = test_bronx$NEIGHBORHOOD, Predicted = rf.predicted))
cm.rpart <- as.matrix(table(Actual = test_bronx$NEIGHBORHOOD, Predicted = rpart.predicted))

## evaluate classification models

n = sum(cm.knn) # number of instances
nc = nrow(cm.knn) # number of classes
diag = diag(cm.knn) # number of correctly classified instances per class 
rowsums = apply(cm.knn, 1, sum) # number of instances per class
colsums = apply(cm.knn, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision = diag / colsums
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

cm.knn
data.frame(recall, precision, f1)

n = sum(cm.rf) # number of instances
nc = nrow(cm.rf) # number of classes
diag = diag(cm.rf) # number of correctly classified instances per class 
rowsums = apply(cm.rf, 1, sum) # number of instances per class
colsums = apply(cm.rf, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision = diag / colsums
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(recall, precision, f1)

n = sum(cm.rpart) # number of instances
nc = nrow(cm.rpart) # number of classes
diag = diag(cm.rpart) # number of correctly classified instances per class 
rowsums = apply(cm.rpart, 1, sum) # number of instances per class
colsums = apply(cm.rpart, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision = diag / colsums
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(recall, precision, f1)




