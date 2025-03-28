
##########################################################################
################### Simple linear regression #############################
##########################################################################

# https://www.dropbox.com/s/fexzvx2m3svhhz2/Income1.csv?dl=0
Income1 <- read.csv("Income1.csv")
View(Income1)

attach(Income1)

mod=lm(Income ~ Education)                   # Model with intercept      
summary(mod)     #standard regression output

print(mod)       #simple printed display
coef(mod)        #(or coefficients()) extracting the regression coefficients
residuals(mod)   #(or resid()) extracting residuals
fitted(mod)      #(or fitted.values()) extracting fitted values
anova(mod)       #comparison of nested models
predict(mod)     #predictions for new data
plot(mod)        #diagnostic plots
confint(mod)     #confidence intervals for the regression coefficients
deviance(mod)    #residual sum of squares
vcov(mod)        #(estimated) variance-covariance matrix
logLik(mod)      #log-likelihood (assuming normally distributed errors)
AIC(mod)         #information criteria including AIC, BIC/SBC (assuming normally distributed errors)

###################################
####### Model With Intercept ######
###################################

mod$fitted.values
mod$residuals

pre <- predict(mod)
yfit = fitted(mod) 

x=Education
y=Income

plot(x, y,xlim=c(-50,100),ylim=c(-50,100))
abline(v=0,h=0, col="grey")
abline(coef(mod), col="red")

plot(x, y)
abline(coef(mod), col="red")
segments(x, y, x, pre, col=3,lty=4)
points(x,yfit,col=3)

############################################
####### Model With Intercept Exercise ######
############################################

attach(cars)
names(cars)

x=speed
y=dist

mod=lm(y ~ x)                                   
summary(mod)

pre <- predict(mod)
yfit=fitted(mod) 

plot(x, y,xlim=c(-50,100),ylim=c(-50,100))
abline(v=0,h=0, col="grey")
abline(coef(mod), col="red")

plot(x, y)
abline(coef(mod), col="red")
segments(x, y, x, pre, col=3,lty=4)
points(x,yfit,col=3)

sum(mod$residuals^2)
deviance(mod)
var(mod$residuals)
mod$df.residual

N=length(y)
1-var(mod$residuals)/var(y)   # R^2
summary(mod)         # check it


###################################
##### Model Without Intercept #####
###################################

attach(Income1)

mod=lm(Income ~ 0 +  Education)

# or
x=Education
y=Income

mod0=lm(y ~ 0 + x)
summary(mod0)    # pay attention to R-squared

modwithintercept=lm(y ~  x)
summary(modwithintercept)  # compare R-squared

mod0$fitted.values
pre <- predict(mod0)
yfit0=fitted(mod0) 

plot(x, y,xlim=c(-50,100),ylim=c(-50,100))
abline(v=0,h=0, col="grey")
abline(0,coef(mod0), col="red")
segments(x, y, x, pre, col=3,lty=4)
points(x,yfit0,col=3)

##############################################################
######## interpreting diagnostic plots in lm #################
##############################################################

attach(Income1)
mod=lm(Income ~ Education)
summary(mod)
plot(mod)

# residuals versus fitted values 
# QQ plot (Normality)
# Scale-Location Plot (Homoscedasticity assumption)
# Residuals vs Leverage Plot (influential cases)

# Testing analytically for observations that potentially
# may influence our model (instead of Residuals vs Leverage Plot)

influence.measures(mod)   # computing many statistics to check for influence.

car::outlierTest(mod)       # Bonferonni p-value to look for the most extreme observation.
# The null for this test is that the observation is NOT an outlier.
# A "small" p-value means the obs is an outlier. 


##########################################################################
################### Multiple linear regression ###########################
##########################################################################

# install.packages("datarium")

library(tidyverse)
library(caret)
library(datarium)
library(ggplot2)
library(ggpubr)
library(dplyr)

# Load the data
data("marketing", package = "datarium")

?marketing
dim(marketing)
head(marketing)

attach(marketing)

# the first step before to build a model
pairs(marketing)
cor(marketing)
str(marketing)
summary(marketing)

# other descriptive statistics
# install.packages("pastecs")
library(pastecs)
des <- stat.desc(marketing)
round(des, 3)

# tables
# few interesting without factors
library(dplyr)
?summarise
marketing %>% group_by(facebook, youtube) %>% summarise(mean=mean(sales))

# try to understand the relationship with Y

gg1=ggplot(marketing, aes(facebook, sales) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) +
  ggtitle("Simple linear regression model") +
  xlab("Facebook Ads") + ylab("Sales")

gg2=ggplot(marketing, aes(newspaper, sales) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)+
  ggtitle("Simple linear regression model") +
  xlab("Newspaper Ads") + ylab("Sales")

gg3=ggplot(marketing, aes(youtube, sales) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)+
  ggtitle("Simple linear regression model") +
  xlab("Youtube Ads") + ylab("Sales")

figure <- ggarrange(gg1,gg2,gg3,
                    labels = c("A", "B", "C"),
                    ncol = 2, nrow = 2)

figure # the relationships seem quite linear

# Build the Additive MLR model
mod <- lm(sales ~ newspaper, data = marketing ) 
summary(mod)

mod <- lm(sales ~ newspaper * facebook, data = marketing ) 
summary(mod)

mod <- lm(sales ~ youtube + facebook + newspaper, data = marketing )  # easy model

# Summarize the model
summary(mod)

par(mfrow=c(2,2))
plot(mod)

##############################################################
############## testing the assumptions #######################
##############################################################

#1# The regression model is linear in parameters

# The crPlots() function analyses if the predictors have a linear
# relationship with the dependent variable. It attempts to model the residuals 
# of one predictor against the dependent variable

# You should see a violet line that models the residuals of the 
# predictor against the dependent variable. The blue dashed line represents
# the line of best fit. If the violet line seems to be similarly linear as
# the blue line, it is good. If the violet line is curved relative to the blue line, maybe
# there is a linearity problem.

library(car)
?crPlots
crPlots(mod) # residuals of the predictor against the dependent variable

##############################################################

#2# The mean of residuals is zero and the residuals are normally distributed

# are residuals normally distributed?
par(mfrow=c(2,2))
plot(mod)   # look at the QQ plot

# checking the mean: is it close to 0?
mean(mod$residuals)

# Testing (seriously) the Normality Assumption of the error term (instead of QQ plot)
library(tseries)
jarque.bera.test(mod$residuals)
shapiro.test(mod$residuals)

library(nortest) # Anderson-Darling normality test
ad.test(mod$residuals)

library(nortest) # Cramer-von Mises normality test
cvm.test(mod$residuals)

library(nortest) #  Lilliefors (Kolmogorov-Smirnov) normality test
lillie.test(mod$residuals)

library(nortest) # Shapiro-Francia normality test
sf.test(mod$residuals)

##############################################################

#3# Homoscedasticity of residuals

# It seems that heteroscedasticity exists.

# Testing analytically for homoscedasticity (instead of Scale-Location Plot)

# The Breush-Pagan test:
# the null hypothesis is that the variance of the residuals is constant
lmtest::bptest(mod)

##############################################################

#4# Independence of the errors -> No autocorrelation of residuals (especially for time series data)

library(ggplot2)

# Method n.1 - Plot
?acf
acf(mod$residuals)  # If the residuals were not autocorrelated, 
# the correlation (Y-axis) from the immediate next line onwards will 
# drop to a near zero value below the dashed blue line (significance level).
plot(mod$residuals)

# Method n.2 - Test for randomness
library(lawstat)
lawstat::runs.test(mod$residuals) # H0: random residuals

# Method n.3 - Durbin-Watson test
library(lmtest)
lmtest::dwtest(mod) # H0: no autocorrelation of residuals

##############################################################