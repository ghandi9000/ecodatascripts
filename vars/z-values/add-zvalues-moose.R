################################################################################
##
##                 Add z-values for all trees on Moosilauke
##
## - Origin is at the center of the plot
##
################################################################################
source("~/work/functions/functions-coordinates.R")
dat <- read.csv("~/work/data/moose/moose-wide.csv")

################################################################################
##
##                          Using rotation matrices
##
################################################################################
library(plyr)
temp <- ddply(dat, .(pplot), function(x) {
    x_offset <- pi/4                                                  # +x-axis is NE
    theta_a <- unique(x$asp) * pi/180 + x_offset - 2*pi
    theta_s <- unique(x$slope) * pi/180
    rmat <- rz(theta_a)                                                  # rotation matrix around the z-axis
    dims <- nrow(x)
    ps <- matrix(c(x$x, x$y, rep(0, dims), rep(1, dims)), ncol = 4)  # points (homogenous)
    A <- t( rmat %*% t(ps) )[, 1:2]                                  # rotated points
    z <- A[,1] * tan(theta_s)                                            # x-value is distance down/up slope
    z[abs(z) < 2e-13] <- 0
    data.frame(tag=x$tag, new_z = z)
})

## Add z-values to dat
temp <- temp[order(temp$pplot, temp$tag), ]
dat <- dat[order(dat$pplot, dat$tag), ]
dat$z <- temp$new_z

## Write data
write.csv(dat, "~/work/data/moose/moose-wide.csv", row.names = FALSE)

## Cleanup
rm(list=ls())
