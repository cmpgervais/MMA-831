###################################
# Code for the Amusement Park Exercise 
# Modified from Chapman & Feit (2015) 
# 
#################################################################

####### Load data from an external link
sat.df <- read.csv("http://goo.gl/HKnl74")
# or load data from an intenal link
sat.df <- read.csv("C:/Users/Chris Gervais/Documents/MMA-831/rintro-chapter7.csv")
# or load data from your computer by browsing
sat.df <- read.csv(file.choose())
####### Display the structure of the R object                   
str(sat.df)

# Try it!: ls()

###### Preliminary Data Inspection

# basic descriptive statistics 
summary(sat.df)

# check basic data suitability
# generalized pair graphs to check for bivariate correlations
install.packages("gpairs") # only run once
library(gpairs)
#require(gpairs)

gpairs(sat.df)

# fix distance
sat.df$logdist <- log(sat.df$distance)
gpairs(sat.df)  



# NOTE: if using RStudio, it can be helpful to "Clear All" plots if a plot
# appears too small or too large; 
# this is a known issue in RStudio with various packages such as corrplot
#
install.packages("corrplot") # only run once
library(corrplot)

corrplot.mixed(cor(sat.df[ , c(2, 4:9)]), upper="ellipse")

# The goal of a satisfaction drivers analysis is to discover relationships between customers'
# satisfaction with features of the service (or product) and their overall experience.
# For example, to what extent is satisfaction with the park's rides related to
# overall experience? Is the relationship strong or weak?
# Bi-variate plots

plot(overall~rides, data=sat.df, xlab="Satisfaction with Rides", ylab="Overall Satisfaction")

# Fitting a model with a single predictor
m1<-lm(overall~rides, data=sat.df)
str(m1)
m1$coefficients
# -94.962 + 1.703*95
summary(m1)

# R2 in the model fit equals the squared correlation for single predictor models
# Let's check that
cor(sat.df$overall, sat.df$rides)^2

# F-statistic compares the model fit to the mean of the outcome variable


plot(overall~rides, data=sat.df,
     xlab="Satisfaction with Rides", ylab="Overall Satisfaction")
abline(m1, col='blue')

####
# confidence intervals by hand:
1.703 - 1.96*(0.106)
1.703 + 1.96*(0.106)

# not at all convenient
# let's have R do it
confint(m1)

##### Checking for Model Assumptions 
par(mfrow=c(2,2)) # partition the screen

plot(m1)

# Take a look at the outliers
sat.df[c(57, 129, 295), ]

## Nonlinear Relationship Example
# Simulate the dataset
x <- rnorm(500)
y <- x^2 + rnorm(500)
toy.model <- lm(y~x)
# Try it!: summary(toy.model)
plot(y~x)
abline(toy.model, col='blue')
plot(toy.model$fitted.values, toy.model$residuals)


m2 <- lm(overall ~ rides + games + wait + clean, data=sat.df)
summary(m2)
# Try it!: plot(m2)

par(mfrow=c(1,1))
install.packages("ggplot2")
install.packages("coefplot")
library(ggplot2)
library(coefplot)
coefplot(m2, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Rating of Feature", 
         xlab="Association with Overall Satisfaction")

# Comparing Models
summary(m1)$r.squared
summary(m2)$r.squared
summary(m1)$adj.r.squared
summary(m2)$adj.r.squared

# Superimpose the two models 

plot(sat.df$overall, fitted(m1), col='red',
     xlim=c(0,100), ylim=c(0,100),
     xlab="Actual Overall Satisfaction", ylab="Fitted Overall Satisfaction")
points(sat.df$overall, fitted(m2), col='blue')
legend("topleft", legend=c("model 1", "model 2"), 
       col=c("red", "blue"), pch=1)

# Statistical comparison of the two models
anova(m1, m2)


##### Prediction

# Manual 

coef(m2)["(Intercept)"] + coef(m2)["rides"]*100 + coef(m2)["games"]*100 + 
  coef(m2)["wait"]*100 + coef(m2)["clean"]*100 

# Matrix

coef(m2)%*%c(1,100, 100, 100, 100)

# Let R do it
predict(m2, sat.df[1:10,]) # predicted overall satisfaction 
fitted(m2)[1:10] # regression output (predicted)


# hold-out prediction for comparison of the two models 

test.df<-sat.df[451:500,]
train.df<-sat.df[1:450,]
m1.train<-lm(overall~rides, data=train.df)
m2.train <- lm(overall ~ rides + games + wait + clean, data=train.df)

summary(m1.train)

m1.test<-predict(m1.train,test.df)
m2.test<-predict(m2.train,test.df)

# visualize the fitted values 

plot(test.df$overall,type='l', col="blue", xlab=NA, ylab=NA)
# Create a title with a red, bold/italic font
title(main="Holdout Prediction", col.main="red", font.main=4)
# Graph trucks with red dashed line and square points
lines(m1.test, pch=22, lty=2, lwd=2,col="red")
lines(m2.test, lty=3,lwd=2, col=rgb(0,0.5,0))
# Label the x and y axes with dark green text
title(xlab="Time", col.lab=rgb(0,0.5,0))
title(ylab="Satisfaction", col.lab=rgb(0,0.5,0))

#general plot help 
?plot
#help for graphical parameters
?par

# Compute R-squared in the test sample 
SSE = sum((test.df$overall - m1.test)^2)
SST = sum((test.df$overall - mean(test.df$overall))^2)
Rsq=1 - SSE/SST

SSE2 = sum((test.df$overall - m2.test)^2)
Rsq2=1 - SSE2/SST

##### standardizing

# by hand ...
(sat.df$rides - mean(sat.df$rides)) / sd(sat.df$rides)
scale(sat.df$rides)

# create sat.std as a standardized version of sat

sat.std <- sat.df[ , -3]  # sat but remove distance
sat.std[ , 3:8] <- scale(sat.std[ , 3:8])
head(sat.std) #display the first few observations 

##### Handling factors (i.e. categorical variables)
m3 <- lm(overall ~ rides + games + wait + clean + 
                   weekend + logdist + num.child, 
         data = sat.std)
summary(m3)

anova(m2,m3)
# Can not treat num.child as a continuous variable b/c it's categorical.
# iow each additional child does not necessarily affect satisfaction the same way
# Need to convert it to factor

sat.std$num.child.factor <- factor(sat.std$num.child)
m4 <- lm(overall ~ rides + games + wait + clean + 
                   weekend + logdist + num.child.factor, 
         data=sat.std)
summary(m4)

sat.std$has.child <- factor(sat.std$num.child > 0)

m5 <- lm(overall ~ rides + games + wait + clean + logdist + has.child, 
         data=sat.std)
summary(m5) # Seems like our simplification did not deteriorate the fit


# ##### Interaction
# m6 <- lm(overall ~ rides + games + wait + clean + 
#                    weekend + logdist + has.child + 
#                    rides:has.child + games:has.child + wait:has.child +
#                    clean:has.child + 
#                    rides:weekend + games:weekend + wait:weekend + clean:weekend, 
#          data=sat.std)
# 
# summary(m6)


##### reduced model - drop insignificant terms
m7 <- lm(overall ~ rides + games + wait + clean + has.child + 
                   wait:has.child,
         data=sat.std)
summary(m7)

# TRY: coef(m7)["wait"] + coef(m7)["wait:has.childTRUE"]

#Try it!
# anova(m5, m7)
# plot(m7)

library(coefplot)

coefplot(m7, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Rating of Feature", 
         xlab="Association with Overall Satisfaction")

