##################################
##################################
# try 50 MC simulation to choose the right model
##################################
##################################
library(tidyverse)
B <- 10000 # Number of generated data at each step

##################################
# function that return the AIC of the models 
# based on random data generated in different ways
##################################
AIC_of_models <- function(M=50, B=10000, LBmean_generation="unif",
                          LB_generation="norm",
                          length_generation="gamma_unif",
                          n_lev = 40){
  
  results.AIC <- matrix(NA,nrow=M, ncol=3)
  max_lb <- 1000
  
  for(j in 1:M){
    if(LBmean_generation=="unif"){
      LowerBoundMeans <- runif(B, 0,max_lb) 
    }else if(LBmean_generation=="weibull"){
      LowerBoundMeans <- 1+100*rweibull(B, 0.5) 
    }
    
    if(LB_generation=="norm"){
      LowerBoundVariances <- 3*rbeta(B, 6,2)
      LowerBounds <- apply(cbind(LowerBoundMeans, LowerBoundVariances), 1,
                           function(x) rnorm(1, x[1],x[2] ) )
    }else if(LB_generation=="beta"){
      alpha <- sample(1:3,B, replace=T)
      LowerBounds <- apply(cbind(LowerBoundMeans, alpha), 1,
                           function(x) 1+100*rbeta(1,x[2], x[1]*(x[2])/x[1] ) )
    }
    
    LowerBounds[LowerBounds<0] = 0
    
    # Upper bounds are generated from the lower bounds plus a certain random number
    if(length_generation=="gamma_unif"){
      Lengths <- 40*rgamma(B, 2) * runif(B, 10, 30) 
    }
    
    UpperBounds <- LowerBounds + Lengths
    
    x_c <- cut(LowerBounds, n_lev)
    y_c <- cut(UpperBounds, n_lev)
    
    ##  Calculate joint counts at cut levels:
    z <- table(x_c, y_c)
    #sistemo z
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
    # the real mean of the last range is not the one in means_x, because it's
    # greater than the max! I take the max as approsimation
    means_y[means_y==max(means_y)] = max.ub
    data <- data.frame("means_x"= means_x, "means_y"= means_y, "freq" = freq)
    # remove no sense values 
    data <- data %>% filter(means_x < means_y)
    # ##  Plot as a 3D histogram:
    # hist3D(z=z, border="black", xlab="Lower Bound", ylab="Upper Bound",
    #        zlab="Frequence", main="Distribution of Ranges")
    # image2D(z=z, border="black", xlab="Lower Bound", ylab="Upper Bound",
    #         main="Distribution of Ranges")
    # poisson model zero inflated
    mZIP.complete <-zeroinfl(freq ~ means_y+means_x , data=data, dist="poisson") 
    # negative binomial
    m.nb <- glm.nb(freq ~ means_y+means_x, data=data)
    # negative binomial zero inflated model 
    m.nb.zeroInfl <- zeroinfl(freq ~ means_y+means_x, dist = "negbin", data=data)
    
    results.AIC[j,] <- c(AIC(mZIP.complete), AIC(m.nb.zeroInfl), AIC(m.nb)) 
    print(paste("passo ", j))
    print(j)
  }
  colnames(results.AIC) <- c("ZIP", "ZINB","NB")
  return(results.AIC)
} 

# DEPRECATED USING LB_generation="norm" and length_generation="gamma_unif"
library(pscl)
library(MASS)
(modelsAIC <- AIC_of_models())
(modelsAIC2 <- AIC_of_models(LBmean_generation="weibull",
                            LB_generation="beta", length_generation="gamma_unif"))
(modelsAIC2 <- AIC_of_models(LBmean_generation="weibull",
                             LB_generation="beta"))
#
#
# The best model is always the zero inflated negative binomial one
