# Load libraries
library(class)
library(caret)
library(cluster)
library(factoextra)
library(GGally)
library(ggplot2)
library(psych)
library(dendextend)
library(colorspace)

# Read dataset
abalone.data <- read.csv("~/GitHub/data-analytics-leer10/lab03/abalone_dataset.csv")

# Add age group column
abalone.data$age.group <- cut(abalone.data$rings, 
                              breaks = c(0, 8, 11, 35), 
                              labels = c("young", "adult", "old"))

# Remove any missing values
abalone.data <- na.omit(abalone.data)

# Fix column names
colnames(abalone.data)[6] <- "shucked_weight"
colnames(abalone.data)[7] <- "viscera_weight"

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

# Make predictions on the test sets
pred1 <- predict(mod1.knn, newdata = test1)
pred2 <- predict(mod2.knn, newdata = test2)

# Create confusion (contingency) matrices
cm1 <- confusionMatrix(pred1, test1$age.group)
cm2 <- confusionMatrix(pred2, test2$age.group)

# Display results
cm1
cm2

# Compare model accuracies
acc1 <- cm1$overall["Accuracy"]
acc2 <- cm2$overall["Accuracy"]

cat("Model 1 Accuracy:", round(acc1, 4), "\n")
cat("Model 2 Accuracy:", round(acc2, 4), "\n")

# Choose better model and corresponding feature set
if (acc2 > acc1) {
  cat("\nModel 2 performed better. Proceeding with feature subset 2.\n")
  best_features <- features2
} else {
  cat("\nModel 1 performed better. Proceeding with feature subset 1.\n")
  best_features <- features1
}


# Find optimal k for best model
train.x <- train[, best_features]
test.x  <- test[, best_features]
train.y <- train$age.group
test.y  <- test$age.group

k_values <- 1:15
accuracies <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_pred <- knn(train = train.x, test = test.x, cl = train.y, k = k)
  cm <- confusionMatrix(knn_pred, test.y)
  accuracies[i] <- cm$overall["Accuracy"]
}

best_k <- k_values[which.max(accuracies)]
cat("\nBest k:", best_k, "with accuracy:", round(max(accuracies), 4), "\n")

# Plot Accuracy vs. k
accuracy_df <- data.frame(k = k_values, Accuracy = accuracies)

ggplot(accuracy_df, aes(x = k, y = Accuracy)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(size = 2, color = "darkorange") +
  geom_vline(xintercept = best_k, linetype = "dashed", color = "red") +
  annotate("text", x = best_k, y = max(accuracies), 
           label = paste("Best k =", best_k), 
           vjust = -1, color = "red", fontface = "bold") +
  labs(title = "kNN Tuning Curve",
       subtitle = "Accuracy vs. Number of Neighbors (k)",
       x = "k (Number of Neighbors)",
       y = "Accuracy") +
  theme_minimal(base_size = 14)

## k-Means and PAM models ##

# Visualize the data for our Abalone model
## psych scatterplot matrix
pairs.panels(abalone.data[,-5],gap = 0,bg = c("pink", "green", "blue")[abalone.data$age.group],pch=21)

## GGally ## psych scatterplot matrix
ggpairs(abalone.data, ggplot2::aes(colour = age.group))

## K-Means ##

train.scaled <- scale(train.x)

fviz_nbclust(train.scaled, kmeans, method = "silhouette") +
  labs(title = "Optimal K for K-Means (Silhouette Method)",
       x = "Number of Clusters (K)",
       y = "Average Silhouette Width")

best_K_kmeans <- 2
kmeans_model <- kmeans(train.scaled, centers = best_K_kmeans, nstart = 25)

fviz_silhouette(silhouette(kmeans_model$cluster, dist(train.scaled))) +
  labs(title = paste("K-Means Silhouette Plot (K =", best_K_kmeans, ")"))

## Optimal k value for PAM ##

fviz_nbclust(train.scaled, pam, method = "silhouette") +
  labs(title = "Optimal K for PAM (Silhouette Method)",
       x = "Number of Clusters (K)",
       y = "Average Silhouette Width")

# Choose best K visually (replace with value from plot)
best_K_pam <- 2
pam_model <- pam(train.scaled, k = best_K_pam)

# Plot silhouette for PAM
fviz_silhouette(pam_model) +
  labs(title = paste("PAM Silhouette Plot (K =", best_K_pam, ")"))
