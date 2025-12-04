library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(caret)

## Set working directory
setwd("~/GitHub/data-analytics-leer10/assignment4/")

## read data in from dataset
coffee <- read_csv("psd_coffee.csv")

## variable distributions/summaries/description
coffee.data <- na.omit(coffee)



summary(coffee.data)

quant_vars <- c(
  "Arabica Production",
  "Bean Exports",
  "Bean Imports",
  "Beginning Stocks",
  "Domestic Consumption",
  "Ending Stocks",
  "Exports",
  "Imports",
  "Other Production",
  "Production",
  "Roast & Ground Exports",
  "Roast & Ground Imports",
  "Robusta Production",
  "Rst,Ground Dom. Consum",
  "Soluble Dom. Cons.",
  "Soluble Exports",
  "Soluble Imports",
  "Total Distribution",
  "Total Supply"
)

## eliminating outliers

for (v in quant_vars) {
  print(
    ggplot(coffee, aes(y = .data[[v]])) +
      geom_boxplot(fill = "tomato", alpha = 0.7) +
      scale_y_continuous(labels = comma) +
      labs(title = paste("Boxplot of", v), y = v)
  )
}

  

for (v in quant_vars) {
  print(
    ggplot(coffee.data, aes(x = .data[[v]])) +
      geom_histogram(bins = 40, fill = "steelblue", alpha = 0.7) +
      scale_x_continuous(labels = comma) +
      labs(title = paste("Distribution of", v), x = v, y = "Count")
  )
}


