library(readr)
library(EnvStats)
library(nortest)
library(ggplot2)

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

gdp <- epi.data$gdp

ggplot(epi.data, aes(x = gdp[,1:160], y = PAR, colour = region)) +
  geom_point()

lin.mod0 <- lm(PAR~gdp,epi.data)

