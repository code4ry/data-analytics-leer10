# Load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(caret)

# set directory
setwd("~/GitHub/data-analytics-leer10/lab05/")

# Load wine data
wine <- read_csv("wine.data", col_names=FALSE)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

# inspect data frame
head(wine)
wine$Type <- as.factor(wine$Type)

split <- nrow(wine)
train.index <- sample(split, 0.7*split)

train <- wine[train.index, ]
test <- wine[-train.index, ]

# feature plot
ggpairs(train, ggplot2::aes(colour = train$Type))


## 1. train SVM model - linear kernels
svm.mod0 <- svm(Type ~ ., data = train, kernel = 'linear')

svm.mod0

train.pred <- predict(svm.mod0, train)

cm = as.matrix(table(Actual = train$Type, Predicted = train.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

## 2. Tuned SVM model - polynomial kernel

gamma.range <- seq(0.1,10, .1)
gamma.range

Cost.range <- seq(1,20, 1)
Cost.range

tuned.svm <- tune.svm(Type~., data = train, kernel = 'polynomial',gamma = gamma.range, cost = Cost.range)
tuned.svm

svm.mod1 <- svm(Type ~ ., data = train, kernel = 'polynomial', gamma = 0.1, cost = 1)

svm.mod1

train.pred <- predict(svm.mod1, train)

cm = as.matrix(table(Actual = train$Type, Predicted = train.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

### Test set prediction ###

## model 1
test.pred <- predict(svm.mod0, test)

cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

## model 2
test.pred <- predict(svm.mod1, test)

cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

### Random Forest and kNN Classification ###

## Random Forest model

rf.mod <- train(Type~., data=train, method="rf")
rf.predicted <- predict(rf.mod, test)
cm <- as.matrix(table(Actual = test$Type, Predicted = rf.predicted))
cm                

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

## kNN model

knn.mod <- train(Type~., data=train, method="knn")
knn.predicted <- predict(knn.mod, test)
cm <- as.matrix(table(Actual = test$Type, Predicted = knn.predicted))
cm                

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)
