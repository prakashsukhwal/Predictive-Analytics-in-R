## Author: Prakash Sukhwal
## May 2021
## R version: 4.0.2
####################################################
# setting up the infrastructure
# uncomment run the code below only if you do not have 'pacman' installed
# install.packages('pacman', repos = "http://cran.us.r-project.org")

# using pacman we load all needed libraries as the code shows below
# automatically installs the libraries we do not have in our computer

pacman::p_load(tidyverse, caret, corrplot, caTools,car, ROCR, e1071)


#####################################################
# set path to folder where raw data is kept
# pay attention to the '/'

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


data %>%
  group_by(subscribed)%>%
  summarise(per = n()/nrow(data))%>%
  ggplot(aes(x=subscribed, y=per, fill = subscribed)) + 
  geom_bar(stat='identity') +
  geom_text(aes(label = round(per, 2)), vjust = 2)

# check for the 0 and 1 using contrasts
#contrasts(data$subscribed)

# if 1 is not base reference then we can change the base to 1
#data$subscribed = factor(data$subscribed,levels=c('0', '1'))

model = glm(subscribed ~ months_residence, data = data, 
                                      family = binomial)
summary(model)

# What does it mean:
#   months_residence  0.028388
# As the duration in months rises we have reason to believe 
# that the likelihood of subscription increases
# 
# 1. Is 0.028388 is the required probability?

# Model Equation:
#   ln(p/(1-p)) = -2.023792 + 0.028388 * (months_residence)
# so for every unit increase in months_residence 
# ln(p/(1-p)) increases by 0.028388 units

# To compute how the odds for subscribing changes as a function 
# of months_residence
exp(coef(model)) 
# 1.02
# Which means increasing the months of stay by 1 month 
# Odds go up by 1.02

data_point = data.frame(months_residence = 50)
data_point
# get the probability from the model
estimate = predict(model, newdata = data_point , 
                      type = 'response')
estimate

record1 = data[1:4,] # take first 4 rows of test
record1

# predict for the probabilities for these 4 test records
pred = predict(model,newdata=record1, type = 'response') 

#gives you z if type is omitted above
pred

#######################################
# provide more info to our model
# add another column: is_female
model2 = glm(subscribed ~ months_residence + is_female, 
             data = data, family = binomial)
summary(model2)

# To compute how the odds for subscribing changes as a function 
# of months_residence and female subscriber
exp(coef(model2))


#################################
# systematic model development

# set initial seed
set.seed(123)

# create a boolean flag to split data
# sample.split from caTools
splitData = sample.split(data$subscribed, SplitRatio = 0.7)

#split_data
# create train and test datasets
train_set = data[splitData,]

nrow(train_set)/nrow(data)

test_set = data[!splitData,]
nrow(test_set)/nrow(data)

colnames(train_set)

#######################################
# train the model
# use train to create our 1st model
# use all independent variables 
model = glm(subscribed ~ ., data = train_set, family = binomial)
# check for multi-collinearity
vif(model)
summary(model)# AIC 152

###################################
# automate mode refinement
model = step(model, trace = F)

summary(model)# AIC 145.5

####################################################
# p-value for the overall model
with(model, pchisq(null.deviance - deviance,
                   df.null - df.residual, lower.tail=F))


####################################################
# test it on the train set
trainPredict = predict(model, newdata = train_set, 
                       type = 'response')

####################################
# convert probabilities into 1 or 0 using threshold/cut-off
# assign 0s or 1s for the values
p_class = ifelse(trainPredict > 0.5, 1,0)

matrix_table = table(train_set$subscribed, p_class)
matrix_table

# Accuracy 
accuracy = sum(diag(matrix_table))/sum(matrix_table)
round(accuracy, 3)

confusionMatrix(table(p_class,train_set$subscribed), positive='1')


# variable importance 
varImp(model)

# test it on the test set
testPredict = predict(model, newdata = test_set, 
                      type = 'response')


p_class = ifelse(testPredict > 0.5, 1,0)

matrix_table = table(test_set$subscribed, p_class)
matrix_table

# Accuracy 
accuracy = sum(diag(matrix_table))/sum(matrix_table)
round(accuracy, 3)

confusionMatrix(table(p_class, test_set$subscribed), positive='1')






##########################################
### using probabilities for business
head(sort(testPredict, decreasing = T),10)


#####################################################
# Create a lift chart
pred = prediction( trainPredict, train_set$subscribed )

perf = performance(pred, "lift", "rpp" )

plot(perf, main="lift curve", 
     xlab = 'Proportion of Customers (sorted prob)')



########################END ##########################

#################################
# ROC Curve
# we can examine many cut-offs visually
# auc: area under the curve
# can be used to compare multiple models drawn on the same chart
pred = prediction( trainPredict, train_set$subscribed )

perf = performance( pred, "tpr", "fpr" )

plot( perf, colorize = TRUE,
      print.cutoffs.at = seq(0,1,0.1), 
      text.adj = c(-0.2, 1.7))

abline(a=0, b=1)

# perf2 = performance( pred, "acc" ) # overall accuraacy
# plot(perf2)

#AUC
pROC::auc(train_set$subscribed, trainPredict)

#############################################################
# Cost based analysis
# when business knows the relative cost of fp or fn
# e.g. let us say the fp are 4 times as costly as fn in a business
cost.perf = performance(pred, "cost", cost.fp = 4, cost.fn = 1)
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

# cost.perf = performance(pred, "cost")
# pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

# area under the curve
# can be used to compare multiple models drawn on the same chart
auc_measure = performance( pred, "auc" )
unlist(slot(auc_measure, 'y.values'))






#####################################################
## lift curve with customer deciles as x axis
# 
# lift <- function(target, prob, groups=10) {
# if(is.factor(target)) target = as.integer(as.character(target))
# if(is.factor(prob)) prob <- as.integer(as.character(prob))
# table = data.frame(cbind(target, prob))
# table[,"decile"] = ntile(-table[,"prob"], groups)
# gaintable = table %>% group_by(decile)  %>%
#   summarise_at(vars(target), funs(total = n(),
#   totalresp=sum(., na.rm = TRUE))) %>%
#   mutate(
#       Cumresp = cumsum(totalresp), 
#       decile_response = (totalresp/total)*100,       
#       gain=Cumresp/sum(totalresp)*100,
#       lift=gain/(decile*(100/groups)),
#       overall_resp = sum(totalresp),
#       total_repondent_examined = sum(total))
# return(gaintable)
# }
# 
# (df = lift(train_set$subscribed , trainPredict, groups = 10))
# 
# plot(df$decile, df$lift, type="l", ylab="cum. lift", xlab="decile")
# 




#################################
# Info Value

train_set$subscribed = as.numeric(as.character(train_set$subscribed))

summary(train_set$subscribed)

IV = Information::create_infotables(data = train_set, y = 'subscribed')

IV$Summary

