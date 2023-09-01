# document by: Prakash C. Sukhwal
# last edited: Mar 15, 2023

# setting up the infrastructure
# uncomment run the code below only if you do not have 'pacman' installed
# install.packages('pacman', repos = "http://cran.us.r-project.org")

pacman::p_load(tidymodels,palmerpenguins, vip,tidyverse, rpart, rattle, ROCR,caret, corrplot,car, relaimpo)

#rattle()

# connect to the data folder/repository
setwd("C:/Users/isspcs/Desktop/workshop-data")

dataset = read.csv("TelcoChurn.csv")
str(dataset)

# set the correct data types
cols = c('customerID', 'region', 'marital', 'retire','gender', 
         'tollfree', 'equip', 'callcard', 'wireless', 'multline', 
         'voice', 'internet', 'callid', 'callwait', 
         'forward', 'confer', 'ebill')
dataset[cols] = lapply(dataset[cols], factor) 


# performing test train split
# set a seed for reproducible results
seed = 42 
set.seed(seed) 
# collect the data indices returned in a list
nobs = nrow(dataset)  

################################
# generate training data

train = sample(nrow(dataset), 0.7*nobs)
length(train)
################################
# generate testing data
test = setdiff(seq_len(nrow(dataset)), train) 

length(test)
################################
# Build the Decision Tree model.
decision_tree_rpart_spec =  decision_tree() %>%
                            set_engine('rpart') %>%
                            set




#############OLD CODE###################
# Build the Decision Tree model.
input = 2:35 # input variables
target= 36 # target variable (last column 'churn')


model1 = rpart(churn ~ .,
               data=dataset[train,c(input, target)],
               method="class",
               parms=list(split="information"),
               control=rpart.control(minsplit=20,
                                     minbucket=7,
                                     maxdepth =4
               ))








