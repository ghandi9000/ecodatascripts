################################################################################
##
##                      Add slope column to moose data
##
## - Slope are calculated looking downhill through the center of the plots
## - Just converts SLOPE8687 from slope percentage to degrees
##
################################################################################


## Conversion: slope percentage ==> slope degrees
slpperc_todeg <- function(slpperc) {
    return ( atan( slpperc / 100) * (360/(2*pi)) )
}

## Read data and add transformed slope column
dat <- read.csv("~/work/data/moose/moose-wide.csv")
dat <- transform(dat, slope = slpperc_todeg(slope8687))

## Check
## all(table(dat$slope8687) == diag(as.matrix(table(dat$slope8687, dat$theta))))

## Write data
write.csv(dat, "~/work/data/moose/moose-wide.csv", row.names = FALSE)

## Cleanup
rm(list=ls())
