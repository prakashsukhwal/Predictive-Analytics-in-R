## Author: Prakash Sukhwal
## Mar 2023
## R version: 4.2.2
####################################################
# setting up the infrastructure
# uncomment run the code below only if you do not have 'pacman' installed
# install.packages('pacman', repos = "http://cran.us.r-project.org")

# using pacman we load all needed libraries as the code shows below
# automatically installs the libraries we do not have in our computer
pacman::p_load(rpart, vip, rpart.plot, pROC, caret)
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
# train test split
set.seed(42)
datasplit = initial_split(cars,prop = 7/10)
train = training(datasplit)
mean(train$Price)
dim(train)
test = testing(datasplit)
dim(test)

#############################
# decision tree
# let the tree grow and we can prune it later
dtree = rpart(Price ~ .,
              data=train,
              method="anova")

# tree is stored in dtree now
# summary will tell you many pieces of information
summary(dtree)

# Let's look at the tree visually
rpart.plot(dtree)

# print the rules generated
print(dtree)

##############################################
# prune the tree using minimum value of xerror
# visually inspect the complexity parameter
plotcp(dtree)
# draw a vertical line at minimum value
abline(v = which.min(dtree$cptable[,'xerror']), lty='dashed', col='red')

bestcp = dtree$cptable[which.min(dtree$cptable[,'xerror']), 'CP']
# prune the tree
dtree_pruned = prune(dtree, cp = bestcp)
summary(dtree_pruned)
rpart.plot(dtree_pruned)


# variable importance can also be seen from summary
vip(dtree) # for a bar chart

# making predictions
# train data performance of pruned regression tree
pred_tr = predict(dtree, train)

# RSquared
## an easier way from caret package
R2(pred_tr, train$Price)# 0.8761


# test data performance of pruned regression tree
pred_te = predict(dtree, test)

# RSquared
## an easier way from caret package
R2(pred_te, test$Price)# 0.8215934 (tree model) and 0.8244 (MLR)

# Calculate errors for the pruned tree
# errors
RMSE(pred_te, test$Price)# tree model: 1471 and MLR: 1205.803
MAE(pred_te, test$Price)










