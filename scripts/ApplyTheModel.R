############################
# Read the data
############################
library(tidyverse)
data1 <- read_csv("/Users/pietro/Desktop/DBSA/PosgreSQL-Sistem-Architecture-Project/RandomData/randomData1.csv")
data1 <- read_csv("/Users/pietro/Desktop/DBSA/PosgreSQL-Sistem-Architecture-Project/RandomData/randomData1.csv")

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
library(pscl)
m.ZINB <- zeroinfl(freq ~ means_y+means_x, dist = "negbin", data=dataNo0)
summary(m.ZINB)
sum(dataNo0$freq==0)/nrow(dataNo0)
export <- as.data.frame(c(m.ZINB$coefficients$zero, m.ZINB$coefficients$count))
colnames(export) <- c("coef")


############################
# Prevision vs observed
############################
library(plotly)
plot_ly(x=means_x,y=means_y, z=freq, type="scatter3d", color=I(2), size=8) 
plot_ly(x=means_x[-(1:(27889-20938))],y=means_y[-(1:(27889-20938))],
        z=predict(m.ZINB, means_x=means_x, means_y=means_y), 
        type="scatter3d",
        size=I(8)) 

row.names(export) <- c("Beta1", "Beta2","Beta3","Beta4","Beta5","Beta6")
# write.table(export,"/Users/pietro/Desktop/DBSA/Progetto/coefficients0.txt" )
