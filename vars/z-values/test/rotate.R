### rotate.R --- 
## Filename: rotate.R
## Description: homogenous rotation matrices for slope, aspect things
## Author: Noah Peart
## Created: Tue Feb 10 12:21:49 2015 (-0500)
## Last-Updated: Tue Feb 10 18:42:00 2015 (-0500)
##           By: Noah Peart
######################################################################

source("~/work/functions/functions-coordinates.R")

################################################################################
##
##               Z-values for slope-corrected permanent plots
##
################################################################################
## Note:
## Slope was taken into account when plots were measured, so the plots are
## designed to be 20x20 projected onto the x-y plane
## Moosilauke plot orientation: +x = NE, +y = SE
## Slopes are looking downhill from top of plot
## Since x-axis is NE, aspects degrees must add -45 degrees for offset from N
offset <- pi/4

## Test on plot 22, aspect of 270 (directly west)
dat <- read.csv("~/work/data/moose/moose-wide.csv")
tst <- dat[dat$pplot == 22, ]  # plot 22
theta_a <- unique(tst$asp) * pi/180 + offset - 2*pi
theta_s <- unique(tst$slope) * pi/180

## Method:
## 1. Rotate about z-axis by theta_a to align slope along x-axis (matrix called A)
## 2. compute z = x-coordinate * sin(theta_s)
rmat <- rz(theta_a)  # rotation matrix
dims <- nrow(tst)
ps <- matrix(c(tst$x, tst$y, rep(0, dims), rep(1, dims)), ncol = 4)  # points
A <- t( rmat %*% t(ps) )[, 1:2]
z <- A[,1] * tan(theta_s)
points <- data.frame(x = ps[,1], y = ps[,2], z = z)

################################################################################
##
##                  Z-values for non-slope corrected plots
##
################################################################################
## Note: this changes x,y coordinates when z values are computed
## Rotation: rotate around z-axis to align slope along x-axis,
## then rotate around y-axis, then rotate back around z-axis
rmat <- rz(theta_a) %*% ry(theta_s) %*% rz(-theta_a) # rotation matrix 
ps <- matrix(c(tst$x, tst$y, rep(0, length(tst$x)), rep(1, length(tst$x))),
             ncol = 4)
new_ps <- apply(ps, 1, function(x) {
    rmat %*% matrix(x)
})
new_ps <- round(t(new_ps), 4)

################################################################################
##
##                                 Visualize
##
################################################################################
library(lattice)
ps <- points[complete.cases(points),]
wireframe(z ~ x*y, data = ps)

################################################################################
##
##                            Apply to all plots
##
################################################################################
library(plyr)
tst <- ddply(dat, .(pplot), function(x) {
    x_offset <- pi/4  # +x-axis is NE
    theta_a <- unique(x$asp) * pi/180 + offset - 2*pi
    theta_s <- unique(x$slope) * pi/180
    rmat <- rz(theta_a)
    dims <- nrow(x)
    ps <- matrix(c(x$x, x$y, rep(0, dims), rep(1, dims)), ncol = 4)  # points
    A <- t( rmat %*% t(ps) )[, 1:2]
    z <- A[,1] * tan(theta_s)
    z[abs(z) < 2e-13] <- 0
    data.frame(slope = x$slope, aspect = x$asp, x = x$x, y= x$y, new_z = z)
})

x <- tst[tst$pplot == 22,]
x2 <- dat[dat$pplot == 22, c("pplot", "slope", "asp", "x", "y", "z")]
