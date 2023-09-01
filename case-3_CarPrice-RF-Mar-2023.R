## Author: Prakash Sukhwal
## Mar 2023
## R version: 4.2.2
####################################################
# setting up the infrastructure
# uncomment run the code below only if you do not have 'pacman' installed
# install.packages('pacman', repos = "http://cran.us.r-project.org")

# using pacman we load all needed libraries as the code shows below
# automatically installs the libraries we do not have in our computer
pacman::p_load(randomForest, vip, rpart.plot, pROC, caret, ROCR)
setwd("C:/Users/isspcs/Desktop/workshop-data")

#####################################################
# read the data file & inspect it's structure
cars = read.csv('cars.csv')
str(cars)

# summary
summary(cars)
#############################################
# set the correct data types
# nominal vars: MetColor and Automatic
cols = c('MetColor', 'Automatic', 'FuelType', 'Doors')
#cars[,cols]

cars[,cols] = lapply(cars[,cols], factor)

str(cars)

#############################
# Random Forest
# Note : If dependent variable is a factor, classification is assumed, 
# otherwise regression is assumed. 
# 
set.seed(42)
rf = randomForest(Price ~ ., 
                  data = cars, 
                  ntree = 500)
print(rf)

# Note: number of variables use the formula
# floor(sqrt(ncol(cars) - 1))

# Parameter Tuning

# 1. stepFactor specifies at each iteration, mtry is inflated (or deflated) by this value
# 2. improve specifies the (relative) improvement in OOB error must be by this much for the search to continue

mtry = tuneRF(cars[-1],cars$Price, ntreeTry=500, 
              stepFactor=1.5, improve=0.01, plot=T)
bestmtry = mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(bestmtry)

# recreate the model with the best value of mtry

set.seed(42)
rf = randomForest(Price ~ ., data = cars, 
                  ntree = 500, 
                  mtry = bestmtry)
print(rf) # % Var explained: 91.22

#Evaluate variable importance
rf$importance
varImpPlot(rf)







