## Author: Prakash Sukhwal
## Mar 2023
## R version: 4.2.2
####################################################
# setting up the infrastructure
# uncomment run the code below only if you do not have 'pacman' installed
# install.packages('pacman', repos = "http://cran.us.r-project.org")

# using pacman we load all needed libraries as the code shows below
# automatically installs the libraries we do not have in our computer
pacman::p_load(rpart, vip, rpart.plot, pROC)
setwd("C:/Users/isspcs/Desktop/workshop-data")

data =  read.csv('coupon_sub.csv')

head(data, 4)

# structure of our data
str(data)

# run a quick summary
summary(data)
names(data)

########################################################
# data type
# set the variables to factors (categorical data)
cols =  c('subscribed', 'is_female', 'is_married', 'colg_edu', 
          'is_professional', 'is_retired', 'is_unemployed', 
          'dual_income', 'have_minors' ,'own_house', 
          'race_white', 'speak_english')

data[,cols] = lapply(data[,cols], as.factor)

str(data)
#############################
# train test split
set.seed(42)
datasplit = initial_split(data,prop = 7/10)
train = training(datasplit)
dim(train)
test = testing(datasplit)
dim(test)
#############################
# decision tree
dtree = rpart(subscribed ~ .,
               data=train,
               method="class", parms=list(split="information"),
               control=rpart.control(minsplit=20,
                                     minbucket=7,
                                     maxdepth =10 ))

# tree is stored in dtree now
# summary will tell you many pieces of information
summary(dtree)

# print the rules generated
print(dtree)

# Let's look at the tree visually
rpart.plot(dtree)

# Complexity table can be printed separately usig command below
# (can vary slightly on 32-bit and 64-bit machines)
# print(dtree$cptable)

# variable importance can be seen from summary
# it can also be printed separately
vi(dtree)
vip(dtree) # for a bar chart


# Train confusion matrix and accuracy
# 94.06 %
train_pr = predict(dtree, newdata=train, type="class")
# a method from caret package can be helpful 
confusionMatrix(table(train_pr,train$subscribed), positive='1')

# train set performance
test_pr = predict(dtree, newdata=test, type="class")
confusionMatrix(table(test_pr,test$subscribed), positive='1')

###############################################
# ROC curves to do a visual comparison of model 
# library pROC
# decision tree and logistic regression (model code at the end of this file)
pred_tree = predict(dtree, newdata=test, type="prob")

# second column has the predicted prob for class 1
(auc_tree = auc(test$subscribed, pred_tree[,2]))
# Area under the curve: 0.9125
# ROC curve
plot(roc(test$subscribed, pred_tree[,2]))

# run the logictic reg code given at the end and then draw the ROC
pred_lg = predict(model, newdata = test, type='response')
(auc_lg = auc(test$subscribed, pred_lg))
# Area under the curve: 0.9821
# ROC curve
plot(roc(test$subscribed, pred_lg))


##############################################
# prune the tree using minimum value of xerror
bestcp = dtree$cptable[which.min(dtree$cptable[,'xerror']), 'CP']
# prune the tree
dtree_pruned = prune(dtree, cp = bestcp)
rpart.plot(dtree_pruned)

pred_prun_tr = predict(dtree_pruned, newdata=test, type="prob")

# second column has the predicted prob for class 1
(auc_tree = auc(test$subscribed, pred_prun_tr[,2]))

####################### EXTRA ####################################

# Logistic regression model for benchmarking

pacman::p_load(tidyverse, caret, MASS, corrplot, caTools,car, ROCR, e1071)

train_set = train # same as dtree above
test_set = test

#######################################
# train the model
# use train to create our 1st model
# use all independent variables 
md = glm(subscribed ~ ., data = train_set, family = binomial)
# check for multi-collinearity
vif(md)
summary(md)# AIC 152

###################################
# automate mode refinement
model = stepAIC(md, trace = F)

summary(model)# AIC 145.5

####################################################
# test it on the train set
trainPredict = predict(model, newdata = train_set, 
                       type = 'response')

####################################
# convert probabilities into 1 or 0 using threshold/cut-off
# assign 0s or 1s for the values
p_class = ifelse(trainPredict > 0.5, 1,0)

confusionMatrix(table(p_class,train_set$subscribed), positive='1')

# variable importance 
varImp(model)

# test it on the test set
testPredict = predict(model, newdata = test_set, 
                      type = 'response')


p_class = ifelse(testPredict > 0.5, 1,0)
confusionMatrix(table(p_class, test_set$subscribed), positive='1')

