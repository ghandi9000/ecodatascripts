### spline_review.R --- 
## Filename: spline_review.R
## Description: 
## Author: Noah Peart
## Created: Mon Mar  2 14:14:00 2015 (-0500)
## Last-Updated: Mon Mar  2 14:26:05 2015 (-0500)
##           By: Noah Peart
######################################################################
library(splines)
dat <- read.csv("~/work/data/moose/moose-long.csv")

## 2nd degree
K = c(14, 20)
plot(cars)
reg = lm(dist ~ bs(speed, knots = c(K), degree = 2), data = cars)
u = seq(4, 25, by = 1)
B = data.frame(speed = u)
Y = predict(reg, newdata = B)
lines(u, Y, lwd = 2, col = "red")

## 1st degree
K = c(14)
reg = lm(dist ~ bs(speed, knots = c(K), degree = 1), data = cars)

B <- function(x, j, n, K) {
    b = 0
    a1 = a2 = 0
    if (((K[j+n+1] > K[j+1]) & (j+n <= length(K)) & (n>0)) ==TRUE) {
        a2 = (K[j+n+1]-x) / (K[j+n+1]-K[j+1])*B(x,j+1,n-1,K)
    }
    if (n == 0) { b=((x > K[j]) & (x <= K[j+1]))*1 }
    if (n > 0) { b = a1+a2 }
    return(b)
}

u=seq(0,1,by=.01)
plot(u,B(u,1,2,c(0,0,.4,1,1,1)),lwd=2,col="red",type="l",ylim=c(0,1))
lines(u,B(u,2,2,c(0,0,.4,1,1,1)),lwd=2,col="blue")
lines(u,B(u,3,2,c(0,0,.4,1,1,1)),lwd=2,col="green")
