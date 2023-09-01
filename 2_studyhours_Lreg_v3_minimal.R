## Author: Prakash Sukhwal
## May 2021
## R version: 4.0.2
####################################################
# setting up the infrastructure
# uncomment run the code below only if you do not have 'pacman' installed
# install.packages('pacman', repos = "http://cran.us.r-project.org")

# using pacman we load all needed libraries as the code shows below
# automatically installs the libraries we do not have in our computer

pacman::p_load(tidyverse, broom)

#####################################################
# set path to folder where raw data is kept
# pay attention to the '/'

setwd("C:/Users/isspcs/Desktop/workshop-data")

#####################################################
# read the data file & inspect it's structure
study = read.csv('student_hours.csv')

dim(study)
names(study)
study$Pass = factor(study$Pass, levels = c(0,1))
str(study)

# how many actual pass/fail\
table(study$Pass)
######################################



#####################################################
# build model
model = glm(Pass ~ Hours, data= study, family='binomial')
summary(model)

# log(p/(1-p)) = -4.0777 + 1.5*(Hours)
# p
#####################################################
#check linearity assumption of logit vs Xi
#predict(model, newdata= study) defaults to linear predictors
probs = predict(model, newdata= study, type='response')
probs
# type = response provides probability values


##################################################
# Extract model results
# study = augment(model,study) 

# also add the probs
study$probability = probs

# get predicted classes from probability values
study$pred = ifelse(study$probability > 0.5, 1, 0)

## Comparing actual vs predicted labels
matrix_table = table(study$Pass, study$pred)
matrix_table
# Accuracy 
accuracy = sum(diag(matrix_table))/sum(matrix_table)
round(accuracy, 3)



#####################################################
## Plot in ggplot
study = read.csv('student_hours.csv')# for plotting
ggplot(study, aes(x=Hours, y=Pass)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, 
              method.args = list(family=binomial)) +
  ylab("Probability of passing exam") + 
  xlab("Hours Studying")


##################################################
# predict on unseen data
test = data.frame(Hours = 4.8)
predict(model, newdata = test, type='response')

######################## END ##########################




########################
# meanings
# .fitted: fitted values
# .resid: residual errors
# .hat: hat values, used to detect high-leverage points (or extreme values in the predictors x variables)
# .std.resid: standardized residuals, which is the residuals divided by their standard errors. Used to detect outliers (or extreme values in the outcome y variable)
# .cooksd: Cook's distance, used to detect influential values, which can be an outlier or a high leverage point
# .sigma: Extract Residual Standard Deviation 
# sigma^2 (sigma(.)^2) is called "dispersion (parameter)". Consequently, for well-fitting binomial or Poisson GLMs, sigma is around 1



#####################################################
### Plot it using data generation
newdat = data.frame(Hours=seq(min(study$Hours),max(study$Hours),
                              len=100))
newdat$Pass = predict(model, newdata=newdat, type="response")

plot(Pass ~ Hours, data=study, col="red4")
lines(Pass ~ Hours, newdat, col="green4", lwd=2)


