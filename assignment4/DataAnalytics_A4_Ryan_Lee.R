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

## Set working directory
setwd("~/GitHub/data-analytics-leer10/assignment4/")

## read data in from dataset
coffee <- read_csv("psd_coffee.csv")

## variable distributions/summaries/description
coffee.data <- na.omit(coffee)
coffee_clean <- coffee[rowSums(coffee[, 3:ncol(coffee)] != 0) > 0, ]

# variables to transform
vars_to_transform <- c("Domestic Consumption", "Production", "Total Supply")

# create log-transformed versions
coffee_log <- coffee_clean %>%
  mutate(
    log_Consumption = log(`Domestic Consumption` + 1),
    log_Production = log(Production + 1),
    log_TotalSupply = log(`Total Supply` + 1)
  )

summary(coffee_log)

## basic data analysis

# global consumption trend
global_trend <- coffee_log %>%
  group_by(Year) %>%
  summarize(GlobalConsumption = sum(log_Consumption, na.rm = TRUE))

ggplot(global_trend, aes(x = Year, y = GlobalConsumption)) +
  geom_line() +
  labs(title = "Global Coffee Consumption Over Time (Log-Transformed)",
       y = "Log(Domestic Consumption)")

# country-level trends
ggplot(coffee_log, aes(Year, log_Consumption, group = Country)) +
  geom_line(alpha = 0.3) +
  labs(title = "Domestic Consumption Across Countries (Log-Transformed)",
       y = "Log(Domestic Consumption)")

# supply - consumption comparison
ggplot(coffee_log, aes(log_TotalSupply, log_Consumption)) +
  geom_point(alpha = 0.4) +
  labs(title = "Relationship Between Supply and Consumption (Log-Transformed)",
       x = "Log(Total Supply)",
       y = "Log(Domestic Consumption)")

# consumption composition
comp <- coffee_log %>%
  group_by(Year) %>%
  summarize(
    RG = sum(log(`Rst,Ground Dom. Consum` + 1), na.rm = TRUE),
    Sol = sum(log(`Soluble Dom. Cons.` + 1), na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(cols = c("RG","Sol"), names_to="Type", values_to="Amount")

ggplot(comp, aes(Year, Amount, color=Type)) +
  geom_line() +
  labs(title = "Roast/Ground vs Soluble Coffee Consumption (Log-Transformed)",
       y = "Log(Amount)")

## exploratory data analysis

# production vs consumption
ggplot(coffee_log, aes(log_Production, log_Consumption)) +
  geom_point(alpha = 0.5) +
  labs(title = "Production vs. Domestic Consumption (Log-Transformed)",
       x = "Log(Production)",
       y = "Log(Domestic Consumption)")

# Log-transformed
ggplot(coffee_log, aes(log_Consumption)) +
  geom_histogram(fill="steelblue", color="black") +
  labs(title="Histogram of Domestic Consumption (Log-Transformed)")

# Q-Q plots for coffee consumption
qqnorm(coffee_log$log_Consumption, 
       main = "Q-Q Plot of Log(Domestic Consumption)")
qqline(coffee_log$log_Consumption, col = "red")


## Linear Regression Model ##
train.indexes <- sample(nrow(global_trend), 0.75*nrow(global_trend))
train <- global_trend[train.indexes, ]
test <- global_trend[-train.indexes, ]

global_lm <- lm(GlobalConsumption ~ Year, data = train)
summary(global_lm)

lm.pred1 <- predict(global_lm, test)

## err = predicted - real
err <- lm.pred1-test$GlobalConsumption

## MAE
abs.err <- abs(err)
mean.abs.err <- mean(abs.err)

## MSE
sq.err <- err^2
mean.sq.err <- mean(sq.err)

## RMSE
sq.err <- err^2
mean.sq.err <- mean(sq.err)
root.mean.sq.err <- sqrt(mean.sq.err)

results <- data.frame(mae=mean.abs.err, mse=mean.sq.err, rmse=root.mean.sq.err)
results

# Prediction vs Actual
test_plot <- data.frame(
  Year = test$Year,
  Actual = test$GlobalConsumption,
  Predicted = lm.pred1
)

ggplot(test_plot, aes(x = Year)) +
  geom_point(aes(y = Actual), color = "blue", size = 2, alpha = 0.7) +
  geom_point(aes(y = Predicted), color = "red", size = 2, alpha = 0.7) +
  geom_line(aes(y = Predicted), color = "red", size = 1) +
  labs(
    title = "Test Set: Actual vs Predicted Global Consumption",
    y = "Global Consumption",
    x = "Year",
    caption = "Blue = Actual, Red = Predicted"
  ) +
  theme_minimal()

# Residual Plot (Test Set)
test_resid <- data.frame(
  Predicted = lm.pred1,
  Residuals = lm.pred1 - test$GlobalConsumption
)

ggplot(test_resid, aes(x = Predicted, y = Residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  labs(
    title = "Test Set Residuals vs Predicted Values",
    x = "Predicted",
    y = "Residual"
  ) +
  theme_minimal()

ggplot() +
  # Training points
  geom_point(data = train, aes(x = Year, y = GlobalConsumption),
             color = "blue", alpha = 0.5) +
  # Fitted line from training model
  geom_smooth(data = train, aes(x = Year, y = GlobalConsumption),
              method = "lm", color = "red", se = TRUE) +
  # Test points
  geom_point(data = test, aes(x = Year, y = GlobalConsumption),
             color = "green", size = 2.5) +
  labs(
    title = "Training Regression Fit with Test Data Overlay",
    x = "Year",
    y = "Global Consumption",
    caption = "Blue = Train | Green = Test | Red = Fit"
  ) +
  theme_minimal()

ggplot(test_plot, aes(x = Actual, y = Predicted)) +
  geom_point(color = "purple", size = 2, alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1.2) +
  labs(
    title = "Actual vs Predicted (Test Set)",
    x = "Actual Consumption",
    y = "Predicted Consumption"
  ) +
  theme_minimal()

## K-Means Clustering on Countries ##

# Convert to wide format: one country per row
country_matrix <- coffee_log %>%
  select(Country, Year, log_Consumption) %>%
  tidyr::pivot_wider(names_from = Year, values_from = log_Consumption)

# Remove countries with missing values
country_matrix <- country_matrix %>% na.omit()

# Extract country names and numeric matrix
country_names <- country_matrix$Country
train.x <- country_matrix %>% select(-Country)

# Scale features
train.scaled <- scale(train.x)

fviz_nbclust(train.scaled, kmeans, method = "silhouette") +
  labs(
    title = "Optimal K for K-Means (Silhouette Method)",
    x = "Number of Clusters (K)",
    y = "Average Silhouette Width"
  )

# Choose best K manually after viewing plot
best_K_kmeans <- 2   # adjust if the plot suggests otherwise

# Fit model
kmeans_model <- kmeans(train.scaled, centers = best_K_kmeans, nstart = 25)

# Silhouette plot
fviz_silhouette(silhouette(kmeans_model$cluster, dist(train.scaled))) +
  labs(title = paste("K-Means Silhouette Plot (K =", best_K_kmeans, ")"))

# Add cluster labels to output
kmeans_clusters <- data.frame(
  Country = country_names,
  Cluster = kmeans_model$cluster
)

fviz_nbclust(train.scaled, pam, method = "silhouette") +
  labs(
    title = "Optimal K for PAM (Silhouette Method)",
    x = "Number of Clusters (K)",
    y = "Average Silhouette Width"
  )

# Choose best K after viewing plot
best_K_pam <- 2  # adjust if needed

# Fit PAM model
pam_model <- pam(train.scaled, k = best_K_pam)

# PAM silhouette plot
fviz_silhouette(pam_model) +
  labs(title = paste("PAM Silhouette Plot (K =", best_K_pam, ")"))

# Add PAM clusters
pam_clusters <- data.frame(
  Country = country_names,
  Cluster = pam_model$clustering
)

# View cluster outputs
head(kmeans_clusters)
head(pam_clusters)


# Combine cluster labels with original log-consumption data
coffee_clustered <- coffee_log %>%
  left_join(kmeans_clusters, by = "Country")

# Compute cluster mean trajectories
cluster_means <- coffee_clustered %>%
  group_by(Cluster, Year) %>%
  summarize(mean_consumption = mean(log_Consumption, na.rm = TRUE))

ggplot(cluster_means, aes(x = Year, y = mean_consumption,
                          color = factor(Cluster), group = Cluster)) +
  geom_line(size = 1.2) +
  labs(
    title = "Average Domestic Consumption Trend by Cluster",
    y = "Average Log(Domestic Consumption)",
    color = "Cluster"
  ) +
  theme_minimal()


