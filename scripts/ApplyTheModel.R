############################
# Read the data
############################
data <- read_csv("/Users/pietro/Desktop/DBSA/Progetto/randomData.csv")

############################
# Prepare data for NBZIP creating the histograms
############################

# it takes 
n_lev <- round(nrow(data)/300)
x_c <- cut(data$LowerBounds, n_lev)
y_c <- cut(data$UpperBounds, n_lev)

##  Calculate joint counts at cut levels:
z <- table(x_c, y_c)

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
# the real mean of the last range is not the one in means_x, because it's
# greater than the max! I take the max as approsimation
means_y[means_y==max(means_y)] = max.ub
d <- data.frame("means_x"= means_x, "means_y"= means_y, "freq" = freq)
dataNo0 <- d %>% filter(means_x < means_y)
############################
# Apply the model 
############################
# neagtive binomial zero inflated model 
m.ZINB <- zeroinfl(freq ~ means_y+means_x, dist = "negbin", data=dataNo0)
summary(m.ZINB)
sum(dataNo0$freq==0)/nrow(dataNo0)
export <- as.data.frame(c(m.ZINB$coefficients$zero, m.ZINB$coefficients$count))
colnames(export) <- c("coef")
row.names(export) <- c("Beta1", "Beta2","Beta3","Beta4","Beta5","Beta6")
write.table(export,"/Users/pietro/Desktop/DBSA/Progetto/coefficients0.txt" )

############################
# Integrate
############################
LBgrid <- seq(min(means_x), 500, length.out=1000)
UBgrid <- seq(min(means_y), 500, length.out=1000)
I0 <- sum(predict(m.ZINB, newdata=data.frame(means_x=LBgrid, means_y=UBgrid) , type="count"))

sum(fitted(m.ZINB))
sum((means_x<500))

