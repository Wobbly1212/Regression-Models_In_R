
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

#5# The predictors and residuals are uncorrelated 

# the first idea we could have is to look  for correlation but it is not a very good idea - quite useless

cor.test(marketing$facebook, mod$residuals)  # H0: correlation is 0 (there is no correlation)
cor.test(marketing$newspaper, mod$residuals)
cor.test(marketing$youtube, mod$residuals)

par(mfrow=c(2,2))

plot(marketing$facebook, mod$residuals)
plot(marketing$newspaper, mod$residuals)
plot(marketing$youtube, mod$residuals)

# this is the problem of endogeneity
# you will see in details in econometrics

# at the end of this file there is an example on how to check for endogeneity

##############################################################

#6# The number of observations must be greater than number of predictors 

# k+2 where k is the number of predictors. 
# This is the minimum required to derive an error term for the model. 
# It won't generally be a very useful or precise model 

# The general rule of thumb (based on stuff in Frank Harrell's book, 
# Regression Modeling Strategies) is that if we expect to be able to detect reasonable-size 
# effects with reasonable power, you need 10-20 observations per parameter (covariate) estimated

##############################################################

#7# Absence of perfect multicollinearity 

# Remove the predictors with the highest VIF and/or Look at the correlation 
# between all variables and keep only one of all highly correlated pairs

library(corrplot)
corrplot(cor(marketing[, -1]))

mod <- lm(sales ~., data=marketing)
car::vif(mod)

##############################################################

# Check Some Assumptions Automatically

library(gvlma)
?gvlma::gvlma
par(mfrow=c(2,2))  # draw 4 plots in same window
gvlma::gvlma(mod)


############################################################

# other tools for diagnostic (instead of plot lm object)

library(ggfortify)
autoplot(mod, which = 1:6, ncol = 3, label.size = 3)

resid_auxpanel(residuals = resid(mod), 
               predicted = fitted(mod), 
               plots = c("resid", "index"))


##############################################################
############# Model with a binary predictor  #################
##############################################################

# Let's add an invented binary predictor to the previous dataset
# Holidays and working days 
# Load the data
data("marketing", package = "datarium")

marketing

dim(marketing)

day_type=c(rep("Holidays",40),rep("Working Days",160))
day_type
day_type=as.factor(day_type)

marketing_new=cbind(marketing,day_type)

head(marketing_new)
levels(day_type)

attach(marketing_new)

mod2 <- lm(sales ~ youtube + facebook + newspaper + day_type)
summary(mod2)

# for now we do not focus on checking the assumptions

#################################################################
### Model with a categorical predictor with many modalities  ####
#################################################################

# Let's add an invented binary predictor to the previous dataset
# South, North, Center, Islands

geo_area=c(rep("South",60),rep("North",60) ,
           rep("Center",60) ,rep("Islands",20))

geo_area=as.factor(geo_area)
levels(geo_area)

marketing_new2=cbind(marketing_new, geo_area)

attach(marketing_new2)
head(marketing_new2)

mod3 <- lm(sales ~ youtube + facebook + newspaper + geo_area)
summary(mod3)

?relevel
geo_area_new=relevel(geo_area, ref="North")

mod3 <- lm(sales ~ youtube + facebook + newspaper + geo_area_new)
summary(mod3)

# for now we do not focus on checking the assumptions

###############################################################################
############# Model with Interaction between numerical predictors #############
###############################################################################

mod4 <- lm(sales ~ youtube + facebook + youtube*facebook)
summary(mod4)

# all the coefficients are statistically significant
# there is a significant interaction between the two predictors

# sales = 8.10 + 0.019*youtube + 0.029*facebook + 0.0009*youtube*facebook

# We can interpret this as an increase in youtube advertising of 1000 dollars 
# is associated with increased sales of 
# (b1 + b3*facebook)*1000 = 19 + 0.9*facebook units. 
# And an increase in facebook advertising of 1000 dollars will be associated 
# with an increase in sales of (b2 + b3*youtube)*1000 = 28 + 0.9*youtube units.



# simply slope analysis

 # install.packages("pequod")

library(pequod)

# mod1 refers to the first moderator variable

mod5 = lmres(sales ~ youtube * facebook,
             centered = c("youtube", "facebook"),
             data = marketing)

slopedist = simpleSlope(mod5, pred = "youtube", mod1 = "facebook")

summary(slopedist)
PlotSlope(slopedist)


# reverse the moderator

mod5b = lmres(sales ~ youtube * facebook,
             centered = c("youtube", "facebook"),
             data = marketing)

slopedist = simpleSlope(mod5b, pred = "facebook", mod1 = "youtube")

summary(slopedist)
PlotSlope(slopedist)
