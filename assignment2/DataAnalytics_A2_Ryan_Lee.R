library(readr)
library(EnvStats)
library(nortest)
library(ggplot2)
library(class)
library(caret)
library(cluster)
library(factoextra)
library(GGally)
library(psych)
library(dendextend)
library(colorspace)

# set working directory (relative path)
setwd("~/GitHub/data-analytics-leer10/lab01/")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

View(epi.data)

# create subsets
PAR <- epi.data$PAR.new
BER <- epi.data$BER.new

# clean the data
NAs <- is.na(PAR)
PAR <- PAR[!NAs]
PAR

NAs <- is.na(BER)
BER <- BER[!NAs]
BER

### Variable Distributions ###

# boxplots
boxplot(PAR, BER, names = c("PAR","BER"))

# PAR histogram
hist(PAR, prob=TRUE)

# add the density lines
lines(density(PAR,na.rm=TRUE,bw="SJ"))
rug(PAR)

# BER histogram
hist(BER, prob=TRUE)

# add the density lines
lines(density(BER, na.rm=TRUE,bw="SJ"))
rug(BER)

# QQ Plot
qqplot(PAR, BER, xlab="Q-Q plot for PAR vs BER")

### Linear Models ###

epi.data <- epi.data[1:160, ]

gdp <- epi.data$gdp

ggplot(epi.data, aes(x = gdp, y = PAR, colour = region)) +
  geom_point()

lin.mod0 <- lm(PAR~gdp,epi.data)

summary(lin.mod0)

## Plot linear model and Residuals
ggplot(epi.data, aes(x = gdp, y = PAR)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod0, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')


epi.data <- epi.data[1:179, ]

gdp <- epi.data$gdp

ggplot(epi.data, aes(x = gdp, y = BER, colour = region)) + 
  geom_point()

lin.mod1 <- lm(BER~gdp,epi.data)

summary(lin.mod1)

## Plot linear model and Residuals
ggplot(epi.data, aes(x = gdp, y = BER)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')


### kNN Classification ###

epi.data <- na.omit(epi.data)

epi.data$region <- as.factor(epi.data$region)

# Train and test split the data
trainIndex <- createDataPartition(epi.data$region, p = 0.7, list=FALSE)
train <- epi.data[trainIndex,]
test <- epi.data[-trainIndex,]

# Create two different subsets
features1 <- c("ECO.new", "BDH.new", "MKP.new")
features2 <- c("PAE.new", "PHL.new", "SHI.new")

# Prepare training/testing data for each subset
train1 <- train[, c(features1, "region")]
test1 <- test[, c(features1, "region")]

train2 <- train[, c(features2, "region")]
test2 <- test[, c(features2, "region")]

# Set up training control
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

# Train kNN models
mod1.knn <- train(region ~ ., data = train1, method = "knn", metric = metric, trControl = control, preProcess=c("center", "scale"))
mod2.knn <- train(region ~ ., data = train2, method = "knn", metric = metric, trControl = control, preProcess=c("center", "scale"))

# Make predictions on the test sets
pred1 <- predict(mod1.knn, newdata = test1)
pred2 <- predict(mod2.knn, newdata = test2)

# Create confusion (contingency) matrices
cm1 <- confusionMatrix(pred1, test1$region)
cm2 <- confusionMatrix(pred2, test2$region)

# Display results
cm1
cm2

# Compare model accuracies
acc1 <- cm1$overall["Accuracy"]
acc2 <- cm2$overall["Accuracy"]

cat("Model 1 Accuracy:", round(acc1, 7), "\n")
cat("Model 2 Accuracy:", round(acc2, 7), "\n")

# Choose better model and corresponding feature set
if (acc2 > acc1) {
  cat("\nModel 2 performed better. Proceeding with feature subset 2.\n")
  best_features <- features2
} else {
  cat("\nModel 1 performed better. Proceeding with feature subset 1.\n")
  best_features <- features1
}
