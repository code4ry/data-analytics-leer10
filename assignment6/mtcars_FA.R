#####################################
#### Factor Analysis with mtCars ####
#####################################

library(GGally)
library(psych)
library(GPArotation)
library(corrplot)

## load data
dataset <- mtcars

features <- c("Miles per gallon",
              "Number of cylinders",
              "Displacement (cu.in.)",
              "Gross horsepower",
              "Rear axle ratio",
              "Weight (1000 lbs)",
              "1/4 mile time",
              "Engine (0=V-shaped, 1=straight)",
              "Transmission (0=automatic, 1=manual)",
              "Number of forward gears",
              "Number of carburetors")

# names(dataset) <- features

## feature-class plots
ggpairs(dataset)

## subset continuous vars
data.cont <- dataset[, c("mpg", "disp", "hp", "drat", "wt", "qsec")]

## correlation matrix
cor.matrix <- cor(data.cont)

print(round(cor.matrix, 2))

# Visualize correlation matrix
corrplot(cor.matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7,
         title = "Correlation Matrix - mtcars Variables",
         mar = c(0,0,2,0))

### Determine number of factors ###

## Scree plot
scree.data <- scree(cor.matrix, factors = FALSE)

parallel.result <- fa.parallel(data.cont, fm = "ml", fa = "fa", 
                               main = "Parallel Analysis Scree Plot")

## Kaiser criterion
eigenvalues <- eigen(cor.matrix)$values
print(round(eigenvalues, 2))

n.factors.kaiser <- sum(eigenvalues > 1)
n.factors.kaiser

# Maximum Likelihood factor analysis
n.factors <- 2

fa.unrotated <- fa(data.cont, nfactors = n.factors, rotate = "none", fm = "ml")

## loadings
print(fa.unrotated$loadings, cutoff = 0.3)

## communality
print(round(fa.unrotated$communality, 2))

## uniqueness
print(round(fa.unrotated$uniquenesses, 2))

## variance accounted for
print(fa.unrotated$Vaccounted)


### Factor rotation

fa.varimax <- fa(data.cont, nfactors = n.factors, rotate = "varimax", fm = "ml")

print(fa.varimax$loadings)

print(fa.varimax$Vaccounted)


### Interpret Factors

loadings.matrix <- fa.varimax$loadings[,]

print(round(loadings.matrix, 2))

## important features for F1
names(which(abs(loadings.matrix[,1]) > 0.5))

## important features for F2
names(which(abs(loadings.matrix[,2]) > 0.5))


### Visualize Factors

# Factor loading plot
fa.diagram(fa.varimax, main = "Factor Analysis Diagram")

# Biplot
biplot.psych(fa.varimax, main = "Factor Analysis Biplot",
             group = NULL, pch = 20)

### Factor Scores
factor.scores <- factor.scores(data.cont, fa.varimax)$scores
colnames(factor.scores) <- c("Power/Size", "Efficiency")

print(round(head(factor.scores, 10), 2))

# Add car names
factor.scores.df <- data.frame(
  Car = rownames(dataset),
  PowerSize = factor_scores[,1],
  Efficiency = factor_scores[,2]
)

# Plot factor scores
plot(factor.scores[,1], factor.scores[,2],
     xlab = "Factor 1: Power/Size",
     ylab = "Factor 2: Efficiency",
     main = "Factor Scores for mtcars",
     pch = 19, col = "steelblue")
text(factor.scores[,1], factor.scores[,2], 
     labels = rownames(dataset), cex = 0.6, pos = 3)
abline(h = 0, v = 0, lty = 2, col = "gray")


############ THE END ############