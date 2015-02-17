################################################################################
##
##                 Add z-values for all trees on Moosilauke
##
## - Origin is at the center of the plot
##
################################################################################
source("~/work/ecodatascripts/vars/z-values/functions.R")
dat <- read.csv("~/work/data/moose/moose-wide.csv")

################################################################################
##
##                          Using rotation matrices
##
################################################################################
library(plyr)
temp <- ddply(dat, .(pplot), function(x) {
    x_offset <- -45                                               # +x-axis is NE
    theta_a <- unique(x$asp)
    theta_s <- unique(x$slope)
    dims <- nrow(x)
    ps <- matrix(c(x$x, x$y, rep(0, dims), rep(1, dims)), ncol = 4)  # points (homogenous)
    z <- zvals(ps, theta_a, theta_s, x_offset, degrees = TRUE)
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

