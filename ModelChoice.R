###########################
###########################
# Find the best model
###########################
###########################
library(tidyverse)

# data preparation
z <- read_csv("./Histogram.csv")
# remove what is not in the domain
z <- z %>% filter(LowerBounds < UpperBounds)

freq <- as.vector(z[1:n_lev,1:n_lev])

max.lb <- max(LowerBounds)
means_x <- rep( (seq(min(LowerBounds), max(LowerBounds), length.out=n_lev) 
                 + max.lb/(n_lev*2) ), n_lev)

# the real mean of the last range is not the one in means_x, because it's
# greater than the max! I take the max as approsimation
means_x[means_x==max(means_x)] = max.lb

max.ub <- max(UpperBounds)
values_in_meansy <- seq(min(UpperBounds), max(UpperBounds), length.out=n_lev)
means_y <- NULL

for(i in 1:n_lev){
  means_y <- c(means_y, rep(values_in_meansy[i], n_lev))
}       

# the real mean of the last range is not the one in means_y, because it's
# greater than the max! I take the max as approsimation
means_y[means_y==max(means_y)] = max.ub
data <- data.frame("means_x"= means_x, "means_y"= means_y, "freq" = freq)


###########################
# Poisson model
###########################
m.P <-glm(freq ~ means_y+means_x, family=poisson, data=data) 
summary(m.P)

# anyway, it looks to have overdispersion:
var(data$freq)
mean(data$freq)

###########################
# Zero inflated Poisson model
###########################
library(pscl)
m.ZIP.complete <-zeroinfl(freq ~ means_y + means_x , data=data, dist="poisson") 
summary(m.ZIP.complete)

m.ZIP.intercept <-zeroinfl(freq ~ means_y+means_x |1 , data=data, dist="poisson") 
summary(m.ZIP.intercetta) # to be excluded

m.ZIP.x <-zeroinfl(freq ~ means_y+means_x |means_x , data=data, dist="poisson") 
summary(m.ZIP.x)

m.ZIP.y <-zeroinfl(freq ~ means_y+means_x |means_y , data=data, dist="poisson") 
summary(m.ZIP.y)

# select the model on base of AIC
print(c(AIC(m.ZIP.complete), AIC(m.ZIP.intercetta), AIC(m.ZIP.x), AIC(m.ZIP.y)))
# the complete model is the best here

###########################
# Negative binomial
###########################

library(MASS)
m.NB <- glm.nb(freq ~ means_y+means_x, data=data)
summary(m.NB)

###########################
# Neagtive binomial zero inflated model 
###########################

m.NBZIP <- zeroinfl(freq ~ means_y+means_x, dist = "negbin", data=data)
summary(m.NBZIP)

###########################
# Results based on AIC
###########################
print(AIC(m.P), AIC(m.ZIP.complete), AIC(m.NB), AIC(m.NBZIP))

# the best model for these data looks to be the zero inflated negative binomial one