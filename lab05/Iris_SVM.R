###############################
### Support Vector Machines ###
###############################


library(caret)
library(e1071)
library(GGally)

## take copy
dataset <- iris

dataset$Species <- as.character(dataset$Species)
# dataset <- dataset[-which(dataset$Species=="versicolor"),]
dataset <- dataset[-which(dataset$Species=="setosa"),]
dataset$Species <- as.factor(dataset$Species)

# ## split train/test
N <- nrow(dataset)
train.indexes <- sample(N,0.8*N)

train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]

## separate x (features) & y (class labels)
X <- dataset[,1:4] 
Y <- dataset[,5]

## feature plots
ggpairs(train, ggplot2::aes(colour = train$Species))

ggplot(train, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()


## train SVM model - linear kernel
svm.mod0 <- svm(Species ~ Petal.Length + Petal.Width, data = train, kernel = 'linear')

svm.mod0

plot(svm.mod0, data = train, formula = Petal.Length~Petal.Width, svSymbol = "x", dataSymbol = "o")


train.pred <- predict(svm.mod0, train)

cm = as.matrix(table(Actual = train$Species, Predicted = train.pred))

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

make.grid = function(X, n = 75) {
  grange = apply(X, 2, range)
  X1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  X2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(Petal.Length = X1, Petal.Width = X2)
}

X <- train[,3:4]
Y <- as.numeric(train$Species)
Y[Y==2] <- -1

xgrid = make.grid(X)
# xgrid[1:10,]

ygrid = predict(svm.mod0, xgrid)

plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)

points(X, col = Y + 3, pch = 19)
points(X[svm.mod0$index,], pch = 5, cex = 2)
# 
# beta = drop(t(svm.mod0$coefs)%*%as.matrix(X)[svm.mod0$index,])
# beta0 = svm.mod0$rho
# 
# plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)
# points(X, col = Y + 3, pch = 19)
# points(X[svm.mod0$index,], pch = 5, cex = 2)
# abline(beta0 / beta[2], -beta[1] / beta[2])
# abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
# abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)

## train SVM model - polynomial kernel
svm.mod1 <- svm(Species ~ Petal.Length+Petal.Width, data = train, kernel = 'radial')

plot(svm.mod1, train, Petal.Width~Petal.Length)

train.pred <- predict(svm.mod1, train)

# xgrid = expand.grid(X1 = px1, X2 = px2)
ygrid = predict(svm.mod1, xgrid)

plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(X, col = Y + 1, pch = 19)
points(X[svm.mod0$index,], pch = 5, cex = 2)
points(X, col = Y + 3, pch = 19)

cm = as.matrix(table(Actual = train$Species, Predicted = train.pred))

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


## Tuned SVM - polynomial
gamma.range <- seq(0.1,10, .1)
gamma.range

Cost.range <- seq(1,20, 1)
Cost.range

tuned.svm <- tune.svm(Species~., data = train, kernel = 'polynomial',gamma = gamma.range, cost = Cost.range)
tuned.svm

svm.mod2 <- svm(Species ~ ., data = train, kernel = 'polynomial', gamma = 0.6, cost = 1)

svm.mod2

train.pred <- predict(svm.mod2, train)

cm = as.matrix(table(Actual = train$Species, Predicted = train.pred))

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

## model 0
test.pred <- predict(svm.mod0, test)

cm = as.matrix(table(Actual = test$Species, Predicted = test.pred))

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

## model 1
test.pred <- predict(svm.mod1, test)

cm = as.matrix(table(Actual = test$Species, Predicted = test.pred))

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
test.pred <- predict(svm.mod2, test)

cm = as.matrix(table(Actual = test$Species, Predicted = test.pred))

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


##########################################

