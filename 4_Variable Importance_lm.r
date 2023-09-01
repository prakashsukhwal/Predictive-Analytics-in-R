## Author: Prakash Sukhwal
## Aug 2022
## R version: 4.0.2
####################################################
# setting up the infrastructure
# uncomment run the code below only if you do not have 'pacman' installed
# install.packages('pacman', repos = "http://cran.us.r-project.org")

# using pacman we load all needed libraries as the code shows below
# automatically installs the libraries we do not have in our computer

pacman::p_load(tidyverse, relaimpo)

#####################################################
# set path to folder where raw data is kept
# pay attention to the '/'
setwd("C:/Users/isspcs/Desktop/workshop-data")

#####################################################
# read the data file & inspect it's structure
diamonds = read.csv('diamonds.csv', stringsAsFactors = T)

str(diamonds)
head(diamonds, 4)

df = diamonds

names(df)
cols = c("cut" ,    "color",   "clarity" )
# convert to categorical

df[cols] = lapply(df[cols], factor)
str(df)

corrplot::corrplot(cor(df[, sapply(df, is.numeric)],
             use="complete.obs"), method = "number", type='upper')

#####################################################
# build model
model1 = lm(price ~ .-x -y -z, data = df)
summary(model1)

### any possible multi-collinearity
car::vif(model1)

# Lindeman, Merenda and Gold (lmg) (R2)
# relaimpo allows to force metrics to sum to 100%  
# This is achieved by the option rela=TRUE

imp = calc.relimp(model1, type = c("lmg"),rela = TRUE) 


(imp = data.frame(lmg = imp$lmg, 
                  vars = names(imp$lmg),
                 row.names =NULL))


#####################################
# visualize the order
imp %>%
ggplot(aes(x = reorder(vars, -lmg), y = lmg)) + 
geom_bar(stat='identity')


###############################################
# variable importance using stepwise regression


# Step-1 
# create an intercept only model
base_model = lm(price ~ 1 , data= df)  

# Step-2 
# create a full model
full_model = lm(price ~ .-x -y -z , data= df) 

# Step-3
# Create a step-wise iteration
# default 'both' with 1000 iterations
step_model = step(base_model, 
                  scope = list(lower = base_model, 
                               upper = full_model),                    
                  trace = 0) 

# Step-4
# collect shortlisted variables
# ignore intercept

var_imp = data.frame(vars =names(step_model[[1]]),
                     values = step_model[[1]],
                     row.names =NULL)%>%
                     arrange(values, desc= T)

var_imp

#####################################
# visualise the order
var_imp %>%
  ggplot(aes(x = reorder(vars, -values), y = values)) + 
  geom_bar(stat='identity')


########################END ##################
