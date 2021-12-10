##################################
##################################
# Generate uniform random range data
##################################
##################################

B <- 50000 # Number of generated data

# the mean for each lower bound is randomly generated from an uniform distr.
max_lb <- 1000
LowerBoundMeans <- runif(B, 0,max_lb) 
# the variance for each lower bound is randomly generated from a beta distr.
LowerBoundVariances <- 3*rbeta(B, 6,2)

LowerBounds <- apply(cbind(LowerBoundMeans, LowerBoundVariances), 1,
                     function(x) rnorm(1, x[1],x[2] ) )
# set to zero all the lower bounds lower than 0
LowerBounds[LowerBounds<0] = 0

# Upper bounds are generated from the lower bounds plus a certain random number
Lengths <- 40*rgamma(B, 2) * runif(B, 1, 3)

UpperBounds <- LowerBounds + Lengths

Ranges <- data.frame("LowerBounds"=LowerBounds,
                     "UpperBounds"=UpperBounds)

# write_csv(Ranges, "./randomData.csv")



##################################
##################################
# Generate multimodal random range data
##################################
##################################
B <- 50000
max_lb <- 1000
LowerBoundMeans1 <- runif(1*B/10, 0*max_lb/10,3*max_lb/10) 
LowerBoundMeans2 <- runif(1*B/10, 3*max_lb/10,3.8*max_lb/10)
LowerBoundMeans3 <- runif(1*B/10, 3.8*max_lb/10,6.5*max_lb/10) 
LowerBoundMeans4 <- runif(1.5*B/10, 6.5*max_lb/10,7.3*max_lb/10) 
LowerBoundMeans5 <- runif(0.5*B/10+1, 7.3*max_lb/10,10*max_lb/10) 
LowerBoundMeans <- c(LowerBoundMeans1, LowerBoundMeans2, LowerBoundMeans3, LowerBoundMeans4,
                     LowerBoundMeans5)
# the variance for each lower bound is randomly generated from a beta distr.
LowerBoundVariances <- rbeta(B, 6,2)

LowerBounds <- apply(cbind(LowerBoundMeans, LowerBoundVariances), 1,
                     function(x) rnorm(1, x[1],x[2] ) )
LowerBounds[LowerBounds<0] = 0

# Upper bounds are generated from the lower bounds plus a certain random number
Lengths <- runif(B, 100, 500) * rgamma(B,2) * 0.9
Lengths[Lengths>1000] = 1000
UpperBounds <- LowerBounds + Lengths

Ranges <- data.frame("LowerBounds"=LowerBounds,
                     "UpperBounds"=UpperBounds)

# write_csv(Ranges, "./randomDataMultimodal.csv")