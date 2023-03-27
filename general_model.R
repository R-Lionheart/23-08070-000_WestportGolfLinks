library(modelr)
library(RConics)
library(tidyverse)

# cubic equation x^3 - 6x^2 + 11x - 6 = 0
b <- c(1,-6, 11, -6)

# roots
x0 <- cubic(b)

# plot
x <- seq(0,4, by=0.001)
y <- b[1]*x^3 + b[2]*x^2 + b[3]*x + b[4]

plot(x,y,type="l")
abline(h=0,v=0)
points(cbind(x0,c(0,0,0)), pch=20,col="red",cex=1.8)


## Mimicking lake michigan model
alpha <- c(46.99, 44.91, 42.34, 40.41, 37.38, 36.49, 36.88, 33.87, 32.81, 31.08)
theta <- c(126, 146, 155, 166, 175, 178, 185, 190, 196, 194)
ro <- c(1.448, 3.377, 2.925, 2.070, 1.873, 1.331, 1.951, 1.304, 0.989, 0.733)


