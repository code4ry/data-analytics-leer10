###########################
#### XGBoost with Iris ####
###########################

library(caret)
library(xgboost)
library(GGally)

## load data
dataset <- iris

## feature-class plots
# ggpairs(dataset, ggplot2::aes(colour = dataset$Species))

## convert class labels to numbers
dataset$Species_num <- as.numeric(iris$Species) - 1

## split train/test
split.rat <- 0.8
train.indexes <- sample(150,split.rat*150)

train.data <- dataset[train.indexes,]
test.data <- dataset[-train.indexes,]

X_train <- as.matrix(train.data[,1:4])
X_test <- as.matrix(test.data[,1:4])

Y_train <- train.data$Species_num
Y_test <- test.data$Species_num

## convert data to DMatrix
dtrain <- xgb.DMatrix(data = X_train, label = Y_train)
dtest <- xgb.DMatrix(data = X_test, label = Y_test)

## train and test model

params <- list(
  objective = "multi:softmax",    # Multiclass classification
  num_class = 3,                   # Number of classes
  max_depth = 3,                   # Maximum tree depth
  eta = 0.3,                       # Learning rate
  eval_metric = "merror"           # Evaluation metric (misclassification error)
)

# Train the model
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,                   # Number of boosting rounds
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 10,      # Stop if no improvement for 10 rounds
  verbose = 1,
  print_every_n = 20
)

# Make predictions
predictions <- predict(xgb_model, dtest)

cm <- as.matrix(table(Actual = Y_test, Predicted = predictions))

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

cm

accuracy = sum(diag)/n
accuracy

precision = diag / colsums
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 


data.frame(recall, precision, f1)


### Train and test multiple models with cross valiadation

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

train.data <- dataset[train.indexes,-6]
test.data <- dataset[-train.indexes,-6]

## train models
mod.rpart <- train(Species~., data=train.data, method="rpart", metric=metric, trControl=control)
mod.knn <- train(Species~., data=train.data, method="knn", metric=metric, trControl=control)
mod.rf <- train(Species~., data=train.data, method="rf", metric=metric, trControl=control)
mod.xgb <- train(Species~., data=train.data, method="xgbTree", metric=metric, trControl=control)

## see results (accuracy)
results <- resamples(list(rpart=mod.rpart, knn=mod.knn, rf=mod.rf, xgb=mod.xgb))
summary(results)


############## THE END ##############