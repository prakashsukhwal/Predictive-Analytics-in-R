## Author: Prakash Sukhwal
## Aug 2022
## R version: 4.1.2
####################################################
pacman::p_load(tidyverse)

# Generate some data
dff = data.frame(x = runif(100000, min=-4, max=4))
dff$y = dff$x

# let us plot it
ggplot(dff, aes(x = x, y = y))+
  geom_line(size = 2) + 
  geom_hline(yintercept=0, linetype="dotted", 
             color = "red", size=2)+
  geom_hline(yintercept=1, linetype="dotted", 
             color = "red", size=2)




# Try-1
# let us use a pair of threshold to restrict values 
# between 0 and 1

dff$cappreds = ifelse(dff$x >1 , 1, 
                        ifelse(dff$x <0, 0, dff$x))

# let us plot it
ggplot(dff, aes(x = x, y = cappreds))+
  geom_line(size = 2) + 
  geom_hline(yintercept=0, linetype="dotted", 
             color = "red", size=2)+
  geom_hline(yintercept=1, linetype="dotted", 
             color = "red", size=2)

# Try-2
# let us use a logit function to restrict values 
# between 0 and 1


lg = function(x){
  logit = exp(x)/(1 + exp(x))
  return(logit)
}

dff$logitx = lg(dff$x)

# let us plot it
ggplot(dff, aes(x = x, y = logitx))+
  geom_line(size=2) + 
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red", size=2)+
  geom_hline(yintercept=1, linetype="dashed", 
             color = "red", size=2)

