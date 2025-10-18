# Load libraries
library(class)
library(caret)
library(cluster)
library(factoextra)

# Read dataset
abalone.data <- read.csv("~/GitHub/data-analytics-leer10/lab03/abalone_dataset.csv")

# Add age group column
abalone.data$age.group <- cut(abalone.data$rings, 
                              breaks = c(0, 8, 11, 35), 
                              labels = c("young", "adult", "old"))

# Remove any missing values
abalone.data <- na.omit(abalone.data)

# Train and test split the data
trainIndex <- createDataPartition(abalone.data$age.group, p = 0.7, list=FALSE)
train <- abalone.data[trainIndex,]
test <- abalone.data[-trainIndex,]

# Create two different subsets
features1 <- c("length", "diameter", "height")
features2 <- c("whole_weight", "shucked_weight", "viscera_weight", "shell_weight")

# Prepare training/testing data for each subset
train1 <- train[, c(features1, "age.group")]
test1 <- test[, c(features1, "age.group")]

train2 <- train[, c(features2, "age.group")]
test2 <- test[, c(features2, "age.group")]

# Set up training control
control <- trainControl(method = "cv", number = 20)
metric <- "Accuracy"

# Train kNN models
mod1.knn <- train(age.group ~ ., data = train1, method = "knn", metric = metric, trControl = control)
mod2.knn <- train(age.group ~ ., data = train2, method = "knn", metric = metric, trControl = control)

# Compare model performance
mod1.knn
mod2.knn

# Try different k values
k_values <- 1:9
results <- list()

for (k in k_values) {
  knn_pred <- knn(train=trainFeatures, test=testFeatures, cl=trainLabels, k=k)
  cm <- confusionMatrix(knn_pred, testLabels)
  results[[paste0("k = ", k)]] <- cm
}

# Print all confusion matrices
results

accuracies <- sapply(results, function(x) x$overall["Accuracy"])
best_k <- k_values[which.max(accuracies)]
cat("Best k:", best_k, "with accuracy:", max(accuracies), "\n")


