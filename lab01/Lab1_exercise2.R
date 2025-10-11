library(readr)
library(EnvStats)


# set working directory (relative path)
setwd("~/GitHub/data-analytics-leer10/lab01/")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

# view dataframe
View(epi.data)

# print summary of variables in dataframe
summary(epi.data$ECO.new)

# print values in variable
epi.data$ECO.new


######## Optional ########
## If you want to reference the variable without using the dataframe:

# attach dataframe
attach(epi.data)

# print values in variable
ECO.new

########################



### Explore Variable ###

ECO <- epi.data$ECO.new

# find NAs in variable - outputs vector of logical values, true if NA, false otherwise
NAs <- is.na(ECO)

ECO[which(NAs)]

# print values in variable 
TKP <- epi.data$TKP.new

TKP

# find NAs inv variable - outputs vector of logical values, true if NA, false otherwise
NAs <- is.na(TKP)

# print NAs
TKP[which(NAs)]

# take subset of NOT NAs from variable
TKP.noNA <- TKP[!NAs]

TKP.noNA

# filter for only values above 30
TKP.above30 <- TKP.noNA[TKP.noNA>30]

TKP.above30

# stats
summary(TKP.above30)

# boxplot of variable(s)
boxplot(ECO, TKP.above30, names = c("ECO","TKP"))


### Histograms ###

# histogram (frequency distribution)
hist(ECO)

# define sequence of values over which to plot histogram
x <- seq(20., 80., 10)

# histogram (frequency distribution) over range
hist(ECO, x, prob=TRUE)

# print estimated density curve for variable
lines(density(ECO,na.rm=TRUE,bw=1.)) # or try bw=“SJ”

# print rug
rug(ECO)

x <- seq(20., 80., 5)

# histogram (frequency distribution) over range
hist(ECO, x, prob=TRUE) 

# print estimated density curve for variable
lines(density(ECO,na.rm=TRUE, bw="SJ"))

# print rug
rug(ECO)


# histogram (frequency distribution) over range
hist(ECO.new, x, prob=TRUE) 

# range
x1<-seq(20,80,1)

# generate probability density values for a normal distribution with given mean and sd
d1 <- dnorm(x1,mean=45, sd=11,log=FALSE)

# print density values
lines(x1,d1)

# generate probability density values for a normal distribution with given mean and sd
d2 <- dnorm(x1,mean=64, sd=11,log=FALSE) 

# print density values
lines(x1,d2) 

# print density values
lines(x1,.5*d2)

### Empirical Cumulative Distribution Function ###

# plot ecdfs
plot(ecdf(ECO), do.points=FALSE, verticals=TRUE) 

plot(ecdf(TKP), do.points=FALSE, verticals=TRUE) 


### Quantile-quantile Plots ###

# print quantile-quantile plot for variable with theoretical normal distribuion
qqnorm(ECO); qqline(ECO)


# print quantile-quantile plot for random numbers from a normal distribution with theoretical normal distribution
x <- rnorm(500)
qqnorm(x); qqline(x)


# print quantile-quantile plot for variable with any theoretical distribution
qqplot(rnorm(180), ECO.new.sub, xlab = "Q-Q plot for norm dsn") 
qqline(ECO.new.sub)

# print quantile-quantile plot for 2 variables
qqplot(EPI, MHP, xlab = "Q-Q plot for EPI vs MHP") 

qqplot(x, ECO, xlab = "Q-Q plot for ECO vs TKP") 
qqline(ECO)

y <- rnorm(500)

qqplot(x, y, xlab = "Q-Q plot for ECO vs TKP") 
qqline(y)


## Statistical Tests

x <- rnorm(500)
y <- rnorm(500)

hist(x)
hist(y)

shapiro.test(x)
shapiro.test(y)

ad.test(x)
ad.test(y)

ks.test(x,y)

wilcox.test(x,y)

var.test(x,y)
t.test(x,y)
