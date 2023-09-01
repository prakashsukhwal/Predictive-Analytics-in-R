
## Author: Prakash Sukhwal
## AUg 2022
## R version: 4.1.2
####################################################
# setting up the infrastructure
# uncomment run the code below only if you do not have 'pacman' installed
# install.packages('pacman', repos = "http://cran.us.r-project.org")

# How to install an old package from tar file?
# install.packages("path to tar file",repos=NULL,type="source")
# eg.
# install.packages("C:/Users/isspcs/Downloads/DMwR_0.4.1.tar.gz",
# repos=NULL, type="source")

# using pacman we load all needed libraries as the code shows below
# automatically installs the libraries we do not have in our computer
pacman::p_load(tidyverse, mice, DMwR, car, caret, ROCR,naniar, caTools)


#####################################################
# set path to folder where raw data is kept
# pay attention to the '/'
setwd("C:/Users/isspcs/Desktop/workshop-data")

#####################################################
# read the data file & inspect it's structure
data = read.csv('risk_factors_cervical_cancer.csv', 
                na.strings = c('?'))

# str(data)
# 858 obs. 36 vars.

data = data %>%
       dplyr::select(Age, Number.of.sexual.partners, First.sexual.intercourse,
  Num.of.pregnancies, Smokes, Smokes..years., Hormonal.Contraceptives, Hormonal.Contraceptives..years.,
  IUD, IUD..years., STDs..number., 
  STDs..Time.since.last.diagnosis, Biopsy)

str(data)

cols_ft = c('Smokes', 'Hormonal.Contraceptives', 'IUD', 'Biopsy')
data[, cols_ft] = lapply(data[, cols_ft], factor)

summary(data)

colMeans(is.na(data))

##################################################
### remove the columns with high % of missings
# STDs..Time.since.last.diagnosis with 91 % missing
data$STDs..Time.since.last.diagnosis = NULL

corrplot::corrplot(cor(data[, sapply(data, is.numeric)],
                       use="complete.obs"), method = "number", type='lower')


###################################################
# impute missing data

cancer_imput= mice(data[, !names(data) %in% "Biopsy"])  
# we can also provide method = ?

cancer = complete(cancer_imput) # taking the first dataset


cancer$Target = data$Biopsy

summary(cancer)

colMeans(is.na(cancer))
head(cancer)

prop.table(table(cancer$Target))
# only 6 % cancer 

#set initial seed
set.seed(123)

# create a boolean flag to split data
splitData = sample.split(cancer$Target, SplitRatio = 0.85)

#split_data
# create train and test datasets
train_set = cancer[splitData,]

nrow(train_set)/nrow(cancer)

test_set = cancer[!splitData,]
nrow(test_set)/nrow(cancer)

dim(train_set)
table(train_set$Target)
prop.table(table(train_set$Target))

# use train to create our 1st model
# use all independent variables 
model = glm(Target ~ ., data = train_set, family = binomial)
summary(model)

# check for multi-collinearity
vif(model)

model = step(model, trace = F)

summary(model)

#exp(cbind(coef(model), confint(model)))

# test it on the train set
trainPredict = predict(model, newdata = train_set, 
                       type = 'response')

# assign 0s or 1s for the values
p_class = ifelse(trainPredict > 0.2, 1,0)

matrix_table = table(train_set$Target, p_class)
matrix_table

confusionMatrix(table(p_class,train_set$Target), positive='1')

# over sample data using SMOTE
# -SMOTE(form, data, perc.over = 100, k = 5, perc.under = 200, ...)

# -form: A formula describing the prediction problem
# -data: A data frame containing the original (unbalanced) data set
# -perc.over: A number that drives the decision of 
# how many extra cases from the minority class are generated (known as over-sampling).
# -k: A number indicating the number of nearest neighbors that are used 
# to generate the new examples of the minority class.
# -perc.under:A number that drives the decision of how many extra cases 
# from the majority classes are selected for each case generated from the minority class (known as under-sampling)


train_set$Target = as.factor(train_set$Target)
table(train_set$Target)
#set initial seed
set.seed(123)
train_sm = SMOTE(Target ~ . ,  train_set, 
                 perc.over = 300, 
                 perc.under = 100)
# try different perc.over  perc.under 
table(train_sm$Target)
dim(train_sm)

prop.table(table(train_sm$Target))

str(train_sm)

# use all independent variables 
model2 = glm(Target ~ ., data = train_sm, family = binomial)


# check for multicollinearity
vif(model2)
summary(model2)

model2 = step(model2, trace = F)

summary(model2)

#exp(cbind(coef(model2), confint(model2)))

# test it on the train set
trainPredict = predict(model2, newdata = train_sm, 
                       type = 'response')

# assign 0s or 1s for the values
p_class = ifelse(trainPredict > 0.5, 1,0)

matrix_table = table(train_sm$Target, p_class)
matrix_table

confusionMatrix(table(p_class,train_sm$Target), positive='1')


table(test_set$Target)

# test it on the test set
testPredict = predict(model2, newdata = test_set, 
                      type = 'response')
p_class2 = ifelse(testPredict > 0.5, 1,0)

matrix_table = table(test_set$Target, p_class2)
matrix_table

# 12/(12+5)# Recall/True Positive Rate/Sensitivity
# 
# 12/(12+179) #pos pred rate or Precision/Hit Rate 

# Prevalence: How often does 1  occur in our total data?
# (5+12)/sum(matrix_table) # prevalence

confusionMatrix(table(p_class2,test_set$Target), positive='1')


########################END ##########################







#########################################################
#### Extra: Genrating Information Value for each variable



str(train_sm)

train_sm$Target = as.numeric(as.character(train_sm$Target))

summary(train_sm$Target)

IV = Information::create_infotables(data = train_sm, y = 'Target')

IV$Summary


