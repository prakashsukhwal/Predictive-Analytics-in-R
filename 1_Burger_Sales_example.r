## Author: Prakash Sukhwal
## May 2023
## R version: 4.2.2
####################################################
# setting up the infrastructure
# uncomment run the code below only if you do not have 'pacman' installed
#install.packages('pacman', repos = "http://cran.us.r-project.org")

# using pacman we load all needed libraries as the code shows below
# automatically installs the libraries we do not have in our computer

pacman::p_load(corrplot, modelr, tidyverse)


#####################################################
# set path to folder where raw data is kept
# pay attention to the '/'

setwd("C:/Users/isspcs/Desktop/workshop-data")

#####################################################
# read the data file & inspect it's structure
bgr = read.csv('Burger_Day1_MRA_Data.csv')

head(bgr,3)

str(bgr)

summary(bgr)

names(bgr)

#####################################################
# correlation between independent vars
# cor(bgr[,c('Price', "Advertising")])

#####################################################
# plotting correlations 
corrplot(cor(bgr[, sapply(bgr, is.numeric)],
                       use="complete.obs"), 
                       method = "number", 
                       type='lower')

names(bgr)
#####################################################
# build model
model = lm(BurgerSales ~ Price + Advertising, data = bgr)

#####################################################
# model summary

summary(model)

# Sales = A + Bx1 + Cx2
# BurgerSales = 277.78 + (-21.77)*Price + 74.39 * Advertising

## extracting some more info from the model object

## 1. predicted values
# one way 
model$fitted.values

# another way
bgr = bgr%>%
  add_predictions(model)


## 2. add residuals
# one way 
model$residuals

# another way
bgr = bgr%>%
  add_residuals(model)

ggplot(bgr, aes(pred, resid)) +
  geom_point()+geom_ref_line(h = 0, col='red')

# 3. RSqr. and Adj-RSqr.
summary(model)$adj.r.squared

summary(model)$r.squared

####### END #########










#####################################################
# compute relative importance of independent vars
# method: calc.relimp
# plot relative importance

rel_imp = calc.relimp(model,rela=TRUE)
plot(rel_imp)



