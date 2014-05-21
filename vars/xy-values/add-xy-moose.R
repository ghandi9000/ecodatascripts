##########################################################################
##
## Add columns 'x' and 'y' to Moosilauke data
## 'x' and 'y' are transformation from original quadrat numberings
##  to coordinate system centered in middle of plot.
## All quadrat numbers outside of range [1,10] are set to NA
##
##########################################################################
source("~/work/ecodatascripts/vars/xy-values/functions.R")
dat <- read.csv("~/work/data/data/moose-wide.csv")

center_quadrats <- function(x) {
    newx = seq(-4.5, 4.5, 1)
    xx = x
    for (i in 1:length(x))
        ifelse(!is.na(x[i]) && x[i] > 0 && x[i] <= 10,
               { xx[i] = newx[x[i]] }, { xx[i] = NA })
    return ( xx )
}

dat$x <- center_quadrats(dat$bqudx)
dat$y <- center_quadrats(dat$bqudy)


