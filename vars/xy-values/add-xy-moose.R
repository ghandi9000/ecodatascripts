##########################################################################
##
## Add columns 'x' and 'y' to Moosilauke data
## 'x' and 'y' are transformation from original quadrat numberings
##  to coordinate system centered in middle of plot.
## All quadrat numbers outside of range [1,10] are set to NA
##
##########################################################################
dat <- read.csv("~/work/data/moose/moose-wide.csv")

##########################################################################
##
## Functions to convert x,y coordinates
##
##########################################################################

## Moosilauke x,y -> centered at origin [-4.5,4.5]
## Convert quadrat labels to be centered at origin of plot
## New locations range from -4.5 to 4.5 in x and y directions
## Note: returns NA for all quadrat numbers outside range [1,10]
center_quadrats <- function(x) {
    newx = seq(-4.5, 4.5, 1)
    xx = x
    for (i in 1:length(x))
        ifelse(!is.na(x[i]) && x[i] > 0 && x[i] <= 10,
               { xx[i] = newx[x[i]] }, { xx[i] = NA })
    return ( xx )
}

## Transform data
## NOTE: any quadrats outside range (0,10) are converted to NA
dat <- transform(dat, x=center_quadrats(dat$bqudx), y=center_quadrats(dat$bqudy))

## Write data
write.csv(dat, "~/work/data/moose/moose-wide.csv", row.names=FALSE)

## Cleanup
rm(list=ls())
