### test-rotations.R --- 
## Filename: test-rotations.R
## Description: Test various aspect angles visually
## Author: Noah Peart
## Created: Fri Feb 13 15:21:39 2015 (-0500)
## Last-Updated: Fri Feb 13 15:58:22 2015 (-0500)
##           By: Noah Peart
######################################################################

source("~/work/ecodatascripts/vars/z-values/functions.R")
aspects <- seq(0, 360, length=4)  # aspect angles

library(rgl)

## single value for each quadrat
qsize <- 4.5  # quadrat size
theta_s <- 45  # using slope of 45 degrees for all samples

## Graphics
mfrow3d(2, 2)
res <- sapply(aspects, function(theta_a) {
    ps <- as.matrix(expand.grid(x = -qsize:qsize, y = -qsize:qsize, z = 0, h = 1))
    ps[,3] <- zvals(ps, theta_a, theta_s)
    plot3d(xyz.coords(ps), type = "s")
    grid3d(side = "z")
    planes3d(c(0,0,1), col="light green", alpha = 0.3)
    abclines3d(0, a = diag(3), col = "gray")
    abclines3d(0,
               a = cos(theta_a*pi/180 - pi/4),
               b = sin(theta_a*pi/180 - pi/4),
               c = 0, col = "red", lwd = 4)  # horizontal line parallel to aspect
    title3d(main = paste("Aspect of", theta_a), xlab = "X", ylab = "Y", zlab = "Z")
})

