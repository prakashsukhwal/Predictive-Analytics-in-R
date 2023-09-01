# document by: Prakash C. Sukhwal
# last edited: Aug 2023

pacman::p_load(tidyverse)

setwd("C:/Users/isspcs/Desktop/workshop-data")

school = read.csv('school.csv')%>%
         select(-c(X))

summary(school)

# Task-1
school$score = (school$read + school$math) / 2 

# Task-2
# is there a linear relationship
# Create the model
m1 =  lm(score ~ income, data = school)
summary(m1)

# Create the base plot
school %>%
        ggplot( aes(x = income, y = score))+
        geom_point() +
# Add the line using the fortified fit data, plotting the x vs. the fitted values
        geom_line(data = fortify(m1), 
                  aes(x = income, y = .fitted))



# Task-3
# Add quadratic income 
# Create the model
m2 =  lm(score ~ income + I(income**2), data = school)
summary(m2)


# Create the base plot
school %>%
        ggplot( aes(x = income, y = score))+
        geom_point() +
        # Add the line using the fortified fit data, plotting the x vs. the fitted values
        geom_line(data = fortify(m1), aes(x = income, y = .fitted))+
        geom_line(data = fortify(m2), aes(x = income, y = .fitted), color='red')


# Task-4

# change in income at lower income levels
# 1 unit from 20 to 21 (in 1000s)

# create test data
test1 =  data.frame(income = c(20, 21))
# perform prediction
pred_score1 = predict(m2, newdata = test1)




# take the difference
diff(pred_score1) # 2.116





# change in income at medium income levels
# 1 unit from 30 to 31 (in 1000s)

# create test data
test2 =  data.frame(income = c(30, 31))
# perform prediction
pred_score2 = predict(m2, newdata = test2)

# take the difference
diff(pred_score2) # 1.27

# change in income at higher income levels
# 1 unit from 50 to 51 (in 1000s)

# create test data
test3 =  data.frame(income = c(50, 51))
# perform prediction
pred_score3 = predict(m2, newdata = test3)

# take the difference
diff(pred_score3) # -0.422


# 







