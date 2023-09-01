## Author: Prakash Sukhwal
## Aug 2023
## R version: 4.2.2
####################################################
# setting up the infrastructure
# uncomment run the code below only if you do not have 'pacman' installed
# install.packages('pacman', repos = "http://cran.us.r-project.org")

# using pacman we load all needed libraries as the code shows below
# automatically installs the libraries we do not have in our computer

pacman::p_load(tidyverse, car, modelr)


#####################################################
# set path to folder where raw data is kept
# pay attention to the '/'

setwd("C:/Users/isspcs/Desktop/workshop-data")

#####################################################
# read the data file & inspect it's structure
sal = read.csv("salaries.csv", stringsAsFactors = T)#Salaries#
str(sal)
head(sal, 4)


lapply(Filter(is.factor,sal), levels)

## contrasts apply only to factors
contrasts(sal$rank)
#############################
# change the base level
sal$rank = factor(sal$rank, levels = c('AsstProf', 'AssocProf', 'Prof'))

contrasts(sal$rank)

contrasts(sal$discipline)
contrasts(sal$sex)

# model

m1 = lm(salary ~ ., data=sal)

vif(m1)

#### model refinement

m2 = lm(salary ~ . -yrs.since.phd, data = sal)
vif(m2)

summary(m2)

# further refine-2
m2 = lm(salary ~ . -yrs.since.phd -yrs.service, data = sal)
summary(m2)



m2 = lm(salary ~ . -yrs.since.phd -yrs.service -sex, data = sal )
summary(m2)
# sal$sex = factor(sal$sex, levels = c('Male', 'Female'))
# contrasts(sal$sex)

##########################
# predict salary for existing data 
# data used for training
# predict using model on new data
predict(m2, newdata = sal)

# OR
sal = sal%>%
  add_predictions(m2)%>%
  add_residuals(m2)

########################## END ##########################









#################################
# stepwise refinement of model
m3= step(m1, trace=0)

vif(m3)

summary(m3)


##########################
# predict salary for new data

(test1 =  data.frame(rank = c('AssocProf', 'Prof'),
                     discipline = c('B', 'A'),
                     yrs.since.phd = c(20, 20),
                     yrs.service = c(24, 21),
                     sex = c('Male', 'Female')))

# predict using model on new data
predict(m3, newdata = test1)

(test1%>%
    add_predictions(m3))

