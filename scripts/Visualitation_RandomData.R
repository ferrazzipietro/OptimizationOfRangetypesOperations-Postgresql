##################################
##################################
# Visualization
##################################
##################################
ranges <- read_csv("/Users/pietro/Desktop/DBSA/Progetto/randomData3.csv")
LowerBounds <- ranges$LowerBounds
UpperBounds <- ranges$UpperBounds
n_lev <- 40
x_c <- cut(LowerBounds, n_lev)
y_c <- cut(UpperBounds, n_lev)

# Calculate joint counts at cut levels:
z <- table(x_c, y_c)

#write_csv(z, "/Users/pietro/Desktop/DBSA/Progetto/Histogram.csv")


# Plot the domain of the data
plot(LowerBounds, UpperBounds, main="Ranges represented in bi-dimensional space",
     pch=16, cex=0.4, col=3, )
# Plot as a 3D histogram:
library(plot3D)
hist3D(z=z, border="black", xlab="Lower Bound", ylab="Upper Bound",
       zlab="Frequence", main="Distribution of Ranges")
# Plot the 2D restriction of the previous hist
image2D(z=z, border="black", xlab="Lower Bound", ylab="Upper Bound",
        main="Distribution of Ranges")

