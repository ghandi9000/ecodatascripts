################################################################################
##
##                 Add z-values for all trees on Moosilauke
##
## - Origin is at the center of the plot
##
################################################################################
source("~/work/ecodatascripts/vars/z-values/functions.R")
dat <- read.csv("~/work/data/moose/moose-wide.csv")

## Transform data
dat <- transform(dat, z = zvals( cbind(dat$x, dat$y), dat$slope*(pi/180), dat$asp*(pi/180) ))

## Write data
write.csv(dat, "~/work/data/moose/moose-wide.csv", row.names = FALSE)

## Cleanup
rm(list=ls())
