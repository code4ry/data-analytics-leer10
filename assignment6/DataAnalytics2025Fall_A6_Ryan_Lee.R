library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(caret)
library(dplyr)
library(EnvStats)
library(nortest)
library(cluster)
library(factoextra)
library(dendextend)
library(corrplot)
library(gridExtra)
library(PCAmixdata)
library(pROC)
library(broom)


# set working directory
setwd("~/GitHub/data-analytics-leer10/assignment6")

bank.data <- read.csv("bank-full.csv", sep = ";")
names(bank.data) <- c("Age", "Job", "Marital", "Education", "Default", "Balance", "Housing", "Loan", "Contact", "Day", "Month", "Duration", "Campaign", "Pdays", "Previous", "Poutcome", "Deposit_Outcome")

## Exploratory Data Analysis

bank.data <- na.omit(bank.data)
summary(bank.data)

bank.data$Deposit_Outcome <- as.factor(bank.data$Deposit_Outcome)

ggplot(bank.data, aes(x = Deposit_Outcome)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Deposit Outcome Distribution",
       x = "Outcome", y = "Count") +
  theme_minimal()

ggplot(bank.data, aes(x = Job, fill = Deposit_Outcome)) +
  geom_bar(position = "fill") +
  labs(title = "Job Type vs Deposit Outcome",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(bank.data, aes(x = Marital, fill = Deposit_Outcome)) +
  geom_bar(position = "fill") +
  labs(title = "Job Type vs Deposit Outcome",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(bank.data, aes(x = Age, fill = Deposit_Outcome)) +
  geom_density(alpha = 0.5) +
  labs(title = "Age Density by Deposit Outcome") +
  theme_minimal()

ggplot(bank.data, aes(x = Age, y = Balance, color = Deposit_Outcome)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Age vs Balance colored by Deposit Outcome",
    x = "Age",
    y = "Balance"
  ) +
  theme_minimal()

categorical_vars <- bank.data %>%
  select(Job, Marital, Education, Default, Housing, Loan, Contact, Month, Poutcome)

numeric_vars <- bank.data %>%
  select(Age, Balance, Day, Duration, Campaign, Pdays, Previous)

cat_plots <- lapply(names(categorical_vars), function(v) {
  ggplot(bank.data, aes_string(x = v)) +
    geom_bar(fill = "darkgreen") +
    labs(title = paste("Count of", v)) +
    theme_minimal()
})

grid.arrange(grobs = cat_plots, ncol = 2)

GGally::ggpairs(bank.data[, c("Age", "Balance", "Duration", "Campaign", "Deposit_Outcome")])

corr_matrix <- cor(numeric_vars)
corrplot(corr_matrix, method = "color",
         type = "upper", tl.col = "black", tl.srt = 45)

### Principal Component Analysis ###
num_vars <- c("Age", "Balance", "Day", "Duration", "Campaign", "Pdays", "Previous")

# Split datasets
bank.num <- bank.data[, num_vars]

## Run PCA on numerical features

# Prepare numeric matrix
bank.num <- scale(bank.num)

# Run PCA
num_principal_components <- princomp(bank.num)
num_principal_components$loadings

### Logistic Regression ###
best_features <- c("Age", "Balance", "Day", "Job", "Marital", "Education", "Default", "Housing",
                   "Loan", "Contact", "Month", "Poutcome", "Deposit_Outcome")
bank.data <- bank.data[, best_features]

bank.data$Deposit_Outcome <- as.factor(bank.data$Deposit_Outcome)

index <- sample(1:nrow(bank.data), 0.75 * nrow(bank.data))
train <- bank.data[index, ]
test  <- bank.data[-index, ]

logit_model <- glm(Deposit_Outcome ~ ., 
                   data = train, 
                   family = binomial)
summary(logit_model)

pred_probs <- predict(logit_model, test, type = "response")
pred_class <- ifelse(pred_probs > 0.5, "yes", "no")
pred_class <- as.factor(pred_class)

roc_obj <- roc(test$Deposit_Outcome, pred_probs)

# Plot ROC curve
plot(roc_obj, col = "blue", lwd = 3, main = "ROC Curve for Logistic Regression")
abline(a = 0, b = 1, lty = 2, col = "gray")

# Print AUC
auc(roc_obj)

coef_df <- tidy(logit_model)

ggplot(coef_df[-1,], aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(color = "steelblue", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Coefficient Plot for Logistic Regression",
       x = "Predictor",
       y = "Coefficient (Log-Odds)") +
  theme_minimal()

cm <- as.matrix(table(Actual = test$Deposit_Outcome, Predicted = pred_class))
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy = sum(diag)/n

precision = diag / colsums
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 


data.frame(recall, precision, f1)

### Decision Tree Classification ###
mod.rpart <- train(Deposit_Outcome~., data=train, method="rpart")

rpart_predicted <- predict(mod.rpart, test)

cm1 <- as.matrix(table(Actual = test$Deposit_Outcome, Predicted = rpart_predicted))
cm1

n = sum(cm1) # number of instances
nc = nrow(cm1) # number of classes
diag = diag(cm1) # number of correctly classified instances per class 
rowsums = apply(cm1, 1, sum) # number of instances per class
colsums = apply(cm1, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy = sum(diag)/n

precision = diag / colsums
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall)

data.frame(recall, precision, f1)

### SVM regression ###  
num_vars <- c("Age", "Balance", "Day", "Deposit_Outcome")
bank.num <- bank.data[, num_vars]

# Train/test split
index <- sample(1:nrow(bank.num), 0.75 * nrow(bank.num))
train <- bank.num[index, ]
test  <- bank.num[-index, ]

# Train SVM model (polynomial kernel - your choice)
svm.mod1 <- svm(
  Deposit_Outcome ~ ., 
  data = train, 
  kernel = 'linear',
  gamma = 0.1, 
  cost = 1
)

svm.mod1

# Predictions on training set
train.pred <- predict(svm.mod1, train)

cm <- table(
  Actual = train$Deposit_Outcome,
  Predicted = train.pred
)
cm <- as.matrix(cm)
cm

# Evaluation metrics
n <- sum(cm)
diag_vals <- diag(cm)
rowsums <- apply(cm, 1, sum)
colsums <- apply(cm, 2, sum)

accuracy <- sum(diag_vals) / n
accuracy

recall <- diag_vals / rowsums
precision <- diag_vals / colsums
f1 <- 2 * precision * recall / (precision + recall)

data.frame(precision, recall, f1)
