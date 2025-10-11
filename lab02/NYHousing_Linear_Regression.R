library(ggplot2)
library(dplyr)

### set working directory
setwd("~/GitHub/data-analytics-leer10/lab02")

### read in data
housing.data <- read.csv("NY-House-Dataset.csv", header = TRUE)

View(housing.data)

#### Data Cleaning ####
summary(housing.data)

# Remove missing values
housing.data <- na.omit(housing.data)

# Remove outliers using IQR for PRICE and PROPERTYSQFT
Q1 <- quantile(housing.data$PRICE, 0.25)
Q3 <- quantile(housing.data$PRICE, 0.75)
IQR_price <- Q3 - Q1
housing.data <- housing.data[
  housing.data$PRICE > (Q1 - 1.5 * IQR_price) &
    housing.data$PRICE < (Q3 + 1.5 * IQR_price),
]

Q1_sqft <- quantile(housing.data$PROPERTYSQFT, 0.25)
Q3_sqft <- quantile(housing.data$PROPERTYSQFT, 0.75)
IQR_sqft <- Q3_sqft - Q1_sqft
housing.data <- housing.data[
  housing.data$PRICE > (Q1 - 1.5 * IQR_sqft) &
    housing.data$PRICE < (Q3 + 1.5 * IQR_sqft),
]

# Optional log transform if PRICE is skewed
housing.data$logPRICE <- log(housing.data$PRICE)

#### Linear Models ####

# Model 1: Price ~ PropertySqFt
model1 <- lm(PRICE ~ PROPERTYSQFT, data = housing.data)
summary(model1)

ggplot(housing.data, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "blue") +
  labs(title = "PRICE vs. PROPERTYSQFT with Linear Fit")

ggplot(model1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Model 1)")

# Model 2: PRICE ~ PROPERTYSQFT + BEDS
model2 <- lm(PRICE ~ PROPERTYSQFT + BEDS, data = housing.data)
summary(model2)

ggplot(housing.data, aes(x = BEDS, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "green") +
  labs(title = "PRICE vs. BEDS with Linear Fit")

ggplot(model2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Model 2)")

# Model 3: PRICE ~ PROPERTYSQFT + BEDS + BATH
model3 <- lm(PRICE ~ PROPERTYSQFT + BEDS + BATH, data = housing.data)
summary(model3)

ggplot(housing.data, aes(x = BATH, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = "PRICE vs. BATH with Linear Fit")

ggplot(model3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Model 3)")

AIC(model1, model2, model3)
BIC(model1, model2, model3)
