##########################################
#### Evaluating Classification Models ####
##########################################

library(caret)

## load data
iris.data <- iris

## separate x (features) & y (class labels)
X <- iris.data[,1:4] 
Y <- iris.data[,5]

## feature boxplots
boxplot(X, main="iris features")

## class label distributions
plot(Y)


## feature-class plots
featurePlot(x=X, y=Y, plot="ellipse")

featurePlot(x=X, y=Y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=X, y=Y, plot="density", scales=scales)

## train/test with cross validation

## split train/test
split.rat <- 0.7
train.indexes <- sample(150,split.rat*150)

train <- data[train.indexes,]
test <- data[-train.indexes,]

control <- trainControl(method="cv", number=20)
metric <- "Accuracy"

## train models
mod.rpart <- train(Species~., data=train, method="rpart", metric=metric, trControl=control)
mod.knn <- train(Species~., data=train, method="knn", metric=metric, trControl=control)
mod.rf <- train(Species~., data=train, method="rf", metric=metric, trControl=control)

## see results (accuracy)
results <- resamples(list(rpart=mod.rpart, knn=mod.knn, rf=mod.rf))
summary(results)


#####################
### kNN with Iris ###
#####################

split.rat <- 0.7
train.indexes <- sample(150,split.rat*150)

train <- data[train.indexes,]
test <- data[-train.indexes,]

mod.knn <- train(Species~., data=train, method="knn")

### 1 round cross-validation
knn.train.true <- train[,5]
knn.test.true <- test[,5]

knn.train.predicted <- predict(mod.knn,train[,-5])
knn.test.predicted <- predict(mod.knn,test[,-5])

train.cm = as.matrix(table(Actual = knn.train.true, Predicted = knn.train.predicted))
train.cm
train.accuracy <- sum(diag(train.cm))/nrow(train)
train.accuracy

test.cm = as.matrix(table(Actual = knn.test.true, Predicted = knn.test.predicted))
test.cm
test.accuracy <- sum(diag(test.cm))/nrow(test)
test.accuracy

### 10 round Monte Carlo cross-validation
n_iters <- 10
split.rat <- 0.7
accuracy.list <- list()

for (i in 1:n_iters) {
  
  train.indexes <- sample(150,split.rat*150)
  
  train <- data[train.indexes,]
  test <- data[-train.indexes,]
  
  knn.test.predicted <- test[,5]
  
  mod.knn <- train(Species~., data=train, method="knn")
  
  knn.test.predicted <- predict(mod.knn,test[,-5])
  
  test.cm = as.matrix(table(Actual = knn.test.true, Predicted = knn.test.predicted))
  
  test.accuracy <- sum(diag(test.cm))/nrow(test)
  
  accuracy.list[[i]] <- test.accuracy
  
}


test.accuracy.avg <- mean(as.numeric(accuracy.list))
test.accuracy.avg



###########################
### Precision/Recall/F1 ###
###########################

split.rat <- 0.7
train.indexes <- sample(150,split.rat*150)

train <- data[train.indexes,]
test <- data[-train.indexes,]

mod.knn <- train(Species~., data=train, method="knn")

knn.predicted <- predict(mod.knn,test[,-5])

cm <- as.matrix(table(Actual = test[,5], Predicted = knn.predicted))

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy = sum(diag)/n
accuracy

precision = diag / colsums
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 


data.frame(recall, precision, f1)


##########################################################