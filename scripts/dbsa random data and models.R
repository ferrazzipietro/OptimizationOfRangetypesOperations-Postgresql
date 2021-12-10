library(tidyverse)


##################################
##################################
# The scope of this part is to find a good bivariate distribution
# of probablity to fit the range data.
##################################
##################################


B <- 50000 # Number of generated data

# the mean for each lower bound is rando# the mean for each lower bound is randomly generatedmly generated
max_lb <- 1000
LowerBoundMeans <- runif(B, 0,max_lb) 
# the variance for each lower bound is randomly generated from a beta distr.
LowerBoundVariances <- 3*rbeta(B, 6,2)

LowerBounds <- apply(cbind(LowerBoundMeans, LowerBoundVariances), 1,
                     function(x) rnorm(1, x[1],x[2] ) )
LowerBounds[LowerBounds<0] = 0

# Upper bounds are generated from the lower bounds plus a certain random number
Lengths <- 40*rgamma(B, 2) * runif(B, 1, 3)

UpperBounds <- LowerBounds + Lengths

Ranges <- data.frame("LowerBounds"=LowerBounds,
                     "UpperBounds"=UpperBounds)

# write_csv(Ranges, "/Users/pietro/Desktop/DBSA/Progetto/randomData.csv")
# see the distribution of data
n_lev <- 40
x_c <- cut(LowerBounds, n_lev)
y_c <- cut(UpperBounds, n_lev)

##  Calculate joint counts at cut levels:
z <- table(x_c, y_c)
##  Plot as a 3D histogram:
library(plot3D)
hist3D(z=z, border="black", xlab="Lower Bound", ylab="Upper Bound",
       zlab="Frequence", main="Distribution of Ranges")
image2D(z=z, border="black", xlab="Lower Bound", ylab="Upper Bound",
        main="Distribution of Ranges")

plot(LowerBounds, UpperBounds, main="Ranges represented in bi-dimensional space",
     pch=16, cex=0.4, col=3, )
# la distribuzione ha come dominio:
# (y>x) tutti i valori superiori alla diagonale;
# 0 < y < k (k max osservato per y)
# 0 <= x <= 1000


##################################
##################################
# generate multimodal data
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

# write_csv(Ranges, "/Users/pietro/Desktop/DBSA/Progetto/randomData3.csv")
# see the distribution of data
n_lev <- 40
x_c <- cut(LowerBounds, n_lev)
y_c <- cut(UpperBounds, n_lev)

##  Calculate joint counts at cut levels:
z <- table(x_c, y_c)
##  Plot as a 3D histogram:
library(plot3D)
hist3D(z=z, border="black", xlab="Lower Bound", ylab="Upper Bound",
       zlab="Frequence", main="Distribution of Ranges")

image2D(z=z, border="black", xlab="Lower Bound", ylab="Upper Bound",
        main="Distribution of Ranges")

plot(LowerBounds, UpperBounds, main="Ranges represented in bi-dimensional space",
     pch=16, cex=0.4, col=3, )



# Posso costruire un modello lineare per prevedere il numero di 
# osservazioni entro una classe.
# Ma perché dovrei essere interessato a prevedere quando nella
# precedente implementazione avevo il numero esatto?
# Perché non devo allocare gli array! Mi basta allocare i 
# parametri del modello lineare, non ragiono più in termini di classi!
# Per poter eludere le classi, ho bisogno di trasformare x_c e y_c
# in variabili numeriche, non più classi, in modo da avere un modello
# a cui, passando un valore puntuale(x,y), ci associ una frequenza.
# Posso scegliere, in modo arbitrario, di convertire le classi su x e y
# nella loro media.



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




# poisson model
m0 <-glm(freq ~ means_y+means_x, family=poisson, data=data) 
summary(m0)
m0$deviance

# poisson model zero inflated
library(pscl)
mZIP.complete <-zeroinfl(freq ~ means_y + means_x , data=data, dist="poisson") 
summary(mZIP.complete)

mZIP.intercetta <-zeroinfl(freq ~ means_y+means_x |1 , data=data, dist="poisson") 
summary(mZIP.intercetta) # to be excluded

m.ZIP.x <-zeroinfl(freq ~ means_y+means_x |means_x , data=data, dist="poisson") 
summary(m.ZIP.x)

m.ZIP.y <-zeroinfl(freq ~ means_y+means_x |means_y , data=data, dist="poisson") 
summary(m.ZIP.y)

print(c(AIC(mZIP.complete), AIC(mZIP.intercetta), AIC(m.ZIP.x), AIC(m.ZIP.y)))
# the complete model is the best here

var(data$freq)
mean(data$freq)

# negative binomial
library(MASS)
m.nb <- glm.nb(freq ~ means_y+means_x, data=data)
summary(m.nb)
AIC(m.nb)

# neagtive binomial zero inflated model 

m.nb.zeroInfl <- zeroinfl(freq ~ means_y+means_x, dist =
               "negbin", data=data)
summary(m.nb.zeroInfl)
wp <- 2 * (m.nb.zeroInfl$loglik - m.nb.zeroInfl$loglik)
pchisq(wp, 6, lower.tail = FALSE)

# check if the formula obtained analitically to evaluate the 
# predicted frequencies work
real.pred1 <- fitted(m.nb.zeroInfl,type="count")
summary(real.pred)
betaC <- m.nb.zeroInfl$coefficients$count
betaZ <- m.nb.zeroInfl$coefficients$zero
xx <- data$means_x
yy <- data$means_y
zz <- exp(betaC[1]+betaC[2]*xx+betaC[3]*yy)*
  (1-exp(betaZ[1]+betaZ[2]*xx+betaZ[3]*yy)/(1+exp(betaZ[1]+betaZ[2]*xx+betaZ[3]*yy)))

median((z - real.pred1)[1:300])
scatter3D(xx, yy, real.pred1)
# They are almost equal! Perfect 
print(paste("NB: ",round(AIC(m.nb)), "    ZIP: ", round(AIC(mZIP.complete)),
            "    ZINB: ", round(AIC(m.nb.zeroInfl))))

# the best model looks to be the NB

##################################
##################################
# try 1000 MC simulation to choose the right model
##################################
##################################

B <- 10000 # Number of generated data

# function to find the best model based on AIC

AIC_of_models <- function(M=1000, B=10000, LBmean_generation="unif",
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
    
    ##  Plot as a 3D histogram:
    hist3D(z=z, border="black", xlab="Lower Bound", ylab="Upper Bound",
           zlab="Frequence", main="Distribution of Ranges")
    image2D(z=z, border="black", xlab="Lower Bound", ylab="Upper Bound",
            main="Distribution of Ranges")
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

library(pscl)
library(MASS)
modelsAIC <- AIC_of_models(M=1)
modelsAIC2 <- AIC_of_models(M=1, LBmean_generation="weibull",
                            LB_generation="beta", length_generation=="gamma_unif")



##################################
##################################
# Visualitation for presentation
##################################
##################################

library(tidyverse)
install.packages("Hmisc")
library(Hmisc)
x <- table(cut2(UpperBounds, m = length(UpperBounds)/4)) %>% as.data.frame
hist(UpperBounds, breaks=c(19,403,661,908,1900), freq=T, col=3)
stripchart(c(1100,1400), pch = 22,cex=2.5, bg = "red",add = TRUE)
stripchart(908, pch =19,cex=1, bg = "black",add = TRUE)
stripchart(1900, pch =19,cex=1, bg = "black",add = TRUE)
text(1050,250,"R LowBound", font=2, col=2)
text(1500,250,"R UpBound", font=2, col=2)
text(850,100,"A", font=2, col=1)
text(1850,100,"B", font=2, col=1)


plot(0:2,0:2, type="n", xlab="LowerBounds",
     ylab="UpperBounds",
     main="Example of approximation of data")
lines(c(0.2,1.8,1.8,0.2,0.2),c(0.2,0.2,1.8,1.8,0.2),
      col=3, pch=2) # confirm square visually
points(1,1, pch=19, col=3, cex=3)
text(1,0.85,"Centre", font=2, col=3)


##################################
##################################
# Parametric distribution of the data
##################################
##################################
# The idea is that instead of allocating histogram, we can assume the data coming from a 
# a certain distribution of probability. In this way, the only things to 
# generate and save in memory are its parameter.

# An idea can be: prob([X Y] = [x y]) = beta*exp(-beta*(y-x)*I(y>x)



#########################
# removing sure 0s
#########################
library(tidyverse)
# remove ranges where the end is befor the start
data <- read_csv("/Users/pietro/Desktop/DBSA/Progetto/RandomData.csv")
dataNo0 <- data %>% filter(LowerBounds < UpperBounds)
n_lev <- 40
x_c <- cut(dataNo0$LowerBounds, n_lev)
y_c <- cut(dataNo0$UpperBounds, n_lev)

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
dataNo0 <- data.frame("means_x"= means_x, "means_y"= means_y, "freq" = freq)



m0NO0 <-glm(freq ~ means_y+means_x, family=poisson, data=dataNo0) 
summary(m0NO0)

m1 <- glm(freq ~ means_y+means_x+ I(means_x^2) + I(means_y^2) + I(means_y^3), family=poisson, data=dataNo0) 
m2 <- glm(freq ~ means_y+means_x+ I(means_x^2) + I(means_y^2) + I(means_x^3), family=poisson, data=dataNo0) 
AIC(m1) # scelgo questo. Se aggiungo anche l'altro peggiora
AIC(m2)


pchisq( 2*(m0NO0$deviance - m1$deviance), 1, lower.tail=F )

# poisson model zero inflated
library(pscl)
mZIP.completeNO0 <-zeroinfl(freq ~ means_y+means_x , data=dataNo0, dist="poisson") 
summary(mZIP.completeNO0)


# the complete model is the best here

var(data$freq)
mean(data$freq)

# negative binomial
library(MASS)
m.nbNO0 <- glm.nb(freq ~ means_y+means_x, data=dataNo0)
summary(m.nbNO0)
AIC(m.nbNO0)

# neagtive binomial zero inflated model 
m.nb.zeroInflNO0 <- zeroinfl(freq ~ means_y+means_x, dist =
                            "negbin", data=dataNo0)

AIC(m0NO0)
print(paste("NB: ",round(AIC(m.nbNO0)), "    ZIP: ", round(AIC(mZIP.completeNO0)),
            "    ZINB: ", round(AIC(m.nb.zeroInflNO0))))


#####################################
#####################################
# 
#####################################
#####################################

