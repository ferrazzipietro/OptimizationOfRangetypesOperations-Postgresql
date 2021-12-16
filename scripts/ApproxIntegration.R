
############################
# Integrate
############################

data <- read_csv("/Users/pietro/Desktop/DBSA/PosgreSQL-Sistem-Architecture-Project/RandomData/randomData3.csv")
data1 <- read_csv("/Users/pietro/Desktop/DBSA/PosgreSQL-Sistem-Architecture-Project/RandomData/randomData1.csv")


# it takes 
n_lev <- round(nrow(data)/300)
x_c <- cut(data$LowerBounds, n_lev)
y_c <- cut(data$UpperBounds, n_lev)
x_c1 <- cut(data1$LowerBounds, n_lev)
y_c1 <- cut(data1$UpperBounds, n_lev)

##  Calculate joint counts at cut levels:
z <- table(x_c, y_c)
z1 <- table(x_c1, y_c1)

freq1 <- as.vector(z1[1:n_lev,1:n_lev])
max.lb1 <- max(data$LowerBounds)
means_x1 <- rep( (seq(min(data1$LowerBounds), max(data1$LowerBounds), length.out=n_lev) 
                  + max.lb1/(n_lev*2) ), n_lev)

freq <- as.vector(z[1:n_lev,1:n_lev])
max.lb <- max(data$LowerBounds)
means_x <- rep( (seq(min(data$LowerBounds), max(data$LowerBounds), length.out=n_lev) 
                 + max.lb/(n_lev*2) ), n_lev)

# the real mean of the last range is not the one in means_x, because it's
# greater than the max! I take the max as approsimation
means_x[means_x==max(means_x)] = max.lb
max.ub <- max(data$UpperBounds)
values_in_meansy <- seq(min(data$UpperBounds), max(data$UpperBounds), length.out=n_lev)
means_y <- NULL
for(i in 1:n_lev){
  means_y <- c(means_y, rep(values_in_meansy[i], n_lev))
} 

means_x1[means_x1==max(means_x1)] = max.lb1
max.ub1 <- max(data1$UpperBounds)
values_in_meansy1 <- seq(min(data1$UpperBounds), max(data1$UpperBounds), length.out=n_lev)
means_y1 <- NULL
for(i in 1:n_lev){
  means_y1 <- c(means_y1, rep(values_in_meansy1[i], n_lev))
}     


# the real mean of the last range is not the one in means_x, because it's
# greater than the max! I take the max as approsimation
means_y[means_y==max(means_y)] = max.ub
d <- data.frame("means_x"= means_x, "means_y"= means_y, "freq" = freq)
dataNo0 <- d %>% filter(means_x < means_y)

means_y1[means_y1==max(means_y1)] = max.ub1
d1 <- data.frame("means_x"= means_x1, "means_y"= means_y1, "freq" = freq1)
dataNo0_1 <- d1 %>% filter(means_x < means_y)



############################
# Apply the model 
############################
# neagtive binomial zero inflated model 
library(pscl)
m.ZINB <- zeroinfl(freq ~ means_y+means_x, dist = "negbin", data=dataNo0)
m.ZINB1 <- zeroinfl(freq ~ means_y+means_x, dist = "negbin", data=dataNo0_1)



# &&
# the used number of points used to approximate the integral depends on how many observation
# we had in that are of the domain:
# len^2 = number of observations in this part of the domain
len1 <- sqrt(sum((data[,2]<500))) %>% round
len2 <- sqrt(sum((data[,1]>550))) %>% round

LBgrid_left <- seq(min(means_x), 500, length.out=len1/2)
UBgrid_left <- seq(min(means_y),500, length.out=len1/2)
grid_left <- expand.grid(LBgrid_left,UBgrid_left)
# keeping only the elemnts in the domain
grid_left <- grid_left[grid_left[,1]<=grid_left[,2],]

newdata=data.frame(means_x=grid_left$Var1, 
                   means_y=grid_left$Var2)
left <- sum(predict(m.ZINB,newdata=newdata, type="response"))

# plot_ly(x=newdata$means_x, 
#         y=newdata$means_y, 
#         z=predict(m.ZINB,newdata=newdata, type="response"),
#         type="scatter3d",
#         size=2) %>% layout(title="Previsons for UB < 500", xlab)

LBgrid_right <- seq(550, max(means_x), length.out=len2/2)
UBgrid_right <- seq(550,max(means_y), length.out=len2/2)
grid_right <- expand.grid(LBgrid_right, UBgrid_right)
grid_right <- grid_right[grid_right[,1]<=grid_right[,2],]
rigth <- sum(predict(m.ZINB, 
                     newdata=data.frame(means_x=grid_right$Var1, 
                                        means_y=grid_right$Var2) , 
                     type="response"))

overlap <- 50000 - left - rigth




# join &&


# the integral is the volume under the product of the two.
# the domain is the intersection of the two domains
LBmaxJoint <- min(max(means_x), max(means_x1))
UBmaxJoint <- min(max(means_y), max(means_y1))
LBminJoint <- max(min(means_x), min(means_x1))
UBminJoint <- max(min(means_y), min(means_y1))

# the leght is choosed to provied a sensefull domain over which evaluate the curve 
LBgrid <- seq(LBminJoint, LBmaxJoint, length.out=200)
UBgrid <- seq(UBminJoint, UBmaxJoint, length.out=200)
grid <- expand.grid(LBgrid, UBgrid)
predictions <- predict(m.ZINB,
                       newdata=data.frame(means_x=grid$Var1, means_y=grid$Var2),
                       type="response")
predictions1 <- predict(m.ZINB1,
                        newdata=data.frame(means_x=grid$Var1, means_y=grid$Var2),
                        type="response")


prod <- predictions * predictions1
sum(prod)
sum(predictions)

