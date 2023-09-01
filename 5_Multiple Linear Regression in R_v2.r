## Author: Prakash Sukhwal
## Aug 2022
## R version: 4.0.2
####################################################
# setting up the infrastructure
# uncomment run the code below only if you do not have 'pacman' installed
# install.packages('pacman', repos = "http://cran.us.r-project.org")

# using pacman we load all needed libraries as the code shows below
# automatically installs the libraries we do not have in our computer
pacman::p_load(tidyverse, caret, corrplot,car, relaimpo)

#####################################################
# set path to folder where raw data is kept
# pay attention to the '/'

setwd("C:/Users/isspcs/Desktop/workshop-data")

############################################################
# Expected theoretical model
# P = f(Age-, Automatic+, CC+, HP+, Doors+?, 
#                    FuelType?, MetColor?, Weight-?)


#####################################################
# read the data file & inspect it's structure
cars = read.csv('cars.csv')
str(cars)

# summary
summary(cars)

names(cars)



#############################################
# set the correct data types
# nominal vars: MetColor and Automatic
cols = c('MetColor', 'Automatic', 'FuelType')
#cars[,cols]

cars[,cols] = lapply(cars[,cols], factor)

str(cars)


unique(cars$Doors)

#########################################
# An example of setting ordered factor
# doors is an ordered variable (can be left as numeric)
# here we set as an ordered factor

cars$Doors = factor(cars$Doors, 
                    levels = c(2,3,4,5))

str(cars)


# check the summary again
summary(cars)

str(cars)


#####################################################
# inspect the distribution of target variable

ggplot(cars,aes(Price)) + geom_histogram() 

# Transform Y
ggplot(cars,aes(log(Price))) + geom_histogram()

# outlier values
outliers = filter(cars, (abs(Price - median(Price)) > 2*sd(Price)))

dim(outliers)

# approach: remove outlier values
cars_new = filter(cars, !(abs(Price - median(Price)) > 2*sd(Price)))

dim(cars_new)
############################################
# how is my Target variable distributed now
#ggplot(cars_new, aes(y=Price)) + geom_boxplot()

ggplot(cars_new,aes(Price)) + geom_histogram()


# cars_new = cars_new[-112,]
# dim(cars_new)

# correlation plots

df = cars_new
#head(df)
corrplot::corrplot(cor(df[, sapply(df, is.numeric)],
             use="complete.obs"), method = "number", type='lower')


# ggplot(cars_new, aes(x = Age, y = Price)) + geom_point()+ 
# geom_smooth(method = "loess")
# 
# ggplot(cars_new, aes(x = KM, y = Price)) + geom_point()+
# geom_smooth(method = "loess")

## you can check other variables similarly

##########################################
# example on using transformations
# students can uncomment and try the snippets

# ggplot(cars_new, aes(x = sqrt(Age), y= Price)) + geom_point()+
# geom_smooth(method = "loess")
# 
# ggplot(cars_new, aes(x = sqrt(KM), y = Price)) + geom_point()+ 
# geom_smooth(method = "loess")

################################################
# put the transformations in the cars data frame
# cols_old = c('Age', 'KM')

# cols = c('s_Age', 's_KM')

# cars_new[, cols] = sapply(cars_new[, cols_old], sqrt)

# head(cars_new,2)
# 
# corrplot(cor(cars_new[, sapply(cars, is.numeric)],
#              use="complete.obs"), method = "number", type='lower')


#####################################################
# build model
# # initial model without the transformed variables
# 
# model1 = lm(Price ~. -s_Age - s_KM, data = cars_new)
# 
# summary(model1)
# 
# vif(model1)
# 
# options(repr.plot.width=8, repr.plot.height=6)
# par(mfrow = c(2, 2))
# 
# plot(model1)
# 
# model_stats = augment(model1)
# head(model_stats)
# 
# # histogram of residuals
# options(repr.plot.width=4, repr.plot.height=3)
# ggplot(model_stats, aes(.resid)) + geom_histogram()

#####################################################
# systematic model building
# set seed
# perform train-test split of data

#set initial seed for reproducibility
set.seed(123)

# collect the data indices returned in a list
inds = createDataPartition(cars_new$Price, p=0.7, 
                           list=F,times=1)

################################
# generate training data
train_set = cars_new[inds,]
nrow(train_set)/nrow(df)

dim(train_set)

################################
# generate testing data
test_set = cars_new[-inds,]
nrow(test_set)/nrow(df)

dim(test_set)

# initial model with the transformed variables s_Age, s_KM
# train_set
model2 = lm(Price ~., data = train_set)

# check multicollinearity: vif from car package
vif(model2)

# if no vif issues
# proceed to check the model summary
# summary(model2)

# remove HP from the model2
model3 = lm(Price ~. -HP, data = train_set)

vif(model3)

summary(model3)



# remove MetColor from the model3
# p-value 
model4 = lm(Price ~. -HP -MetColor, 
            data = train_set)

summary(model4)

# remove Doors from the model4
model5 = lm(Price ~. -HP -MetColor -Doors, 
            data = train_set)

summary(model5)

# remove Automatic from the model5
model6 = lm(Price ~. -HP -MetColor -Doors -Automatic,
            data = train_set)

summary(model6)


# relative importance of various predictors in predicting alcohol
imp = calc.relimp(model6, type = c("lmg"),rela = TRUE) 


(imp = data.frame(lmg = imp$lmg, 
                  vars = names(imp$lmg),
                  row.names =NULL))


#####################################
# visualize the order
imp %>%
  ggplot(aes(x = reorder(vars, -lmg), y = lmg)) + 
  geom_bar(stat='identity')

# plot(calc.relimp(model5,rela=TRUE))

####################
# model diagnostic plots

par(mfrow = c(2, 2))

plot(model5)

dim(test_set)

###########################
# testing 
# test the learned model on test data
# use function: predict()

predictTest = predict(model6, newdata=test_set)

predictTest

################################
## goodness of fit: R-Square
## use the basics from class
SSE = sum((test_set$Price - predictTest)^2)

SST = sum((test_set$Price - mean(df$Price))^2)

R_Sq= 1 - (SSE / SST)
round(R_Sq,4)

## an easier way from caret package
R2(predictTest, test_set$Price)

# errors
RMSE(predictTest, test_set$Price)
MAE(predictTest, test_set$Price)


################
### auto tune
model7 = stepAIC(model2)

summary(model7)

###########################
# testing 
# test the learned model on test data
# use function: predict()

predictTest = predict(model7, newdata=test_set)

################################
## goodness of fit: R-Square
## use the basics from class
SSE = sum((test_set$Price - predictTest)^2)
SST = sum((test_set$Price - mean(df$Price))^2)
R_Sq= 1 - (SSE / SST)
round(R_Sq,4)



########################END ##################















#sort(unique(cars$HP))

cars_new$binHP[cars_new$HP < 80 & cars_new$HP >=0 ] = 1
cars_new$binHP[cars_new$HP < 100 & cars_new$HP >=80 ] = 2
cars_new$binHP[cars_new$HP >=100 ] = 3

cars_new$binHP = factor(cars_new$binHP, levels = c(1,2,3), ordered = F)

#set initial seed for reproducibility
set.seed(123)

# collect the data indices returned in a list
inds = createDataPartition(cars_new$Price, p=0.7, list=FALSE,times=1)

train_set = cars_new[inds,]
nrow(train_set)/nrow(df)

dim(train_set)

test_set = cars_new[-inds,]
nrow(test_set)/nrow(df)

modelA = lm(Price ~. -Age - KM -HP , data = train_set)

summary(modelA)
