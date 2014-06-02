################################################################################
##
##                         Test z-value calculations
##
################################################################################
source("~/work/ecodatascripts/vars/z-values/functions.R")
dat <- read.csv("~/work/data/moose/moose-wide.csv")
dat$slprad <- (2*pi/360) * dat$slope # convert slope to radians
dat$aspect <- (2*pi/360) * dat$asp   # convert aspect to radians

## test data
## NOTE: the aspect of this plot is directly west, so its easy to see what the
##  slope should look like
tst <- dat[dat$pplot == 22,]

################################################################################
##
##                               test z-values
##
################################################################################
## Calculate a z-value for every quadrat using simulated data
## Y-axis NW-->SE, so with a slope aspect of 315 degrees, the slope should be directly up the
##  y-axis, i.e. downhill from NW --> SE
showslope(33, 315)

## Slope downhill from N --> S, theta_S = 360 degrees (or 0 degrees, either works)
showslope(33, 360)

## Plot z-values for test plot
showslope(33, 270)

## Plot real data from tst plot where slope is downhill directly west
plotzvals(tst, slope = "slprad", aspect = "aspect")



