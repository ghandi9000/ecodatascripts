## Test z-value calculations

source("~/work/data/scripts/zvalues/functions.R")
dat <- read.csv("~/work/data/data/moose-wide.csv")
dat$slope <- 2*pi/360 * dat$slope8687 # convert slope to radians
dat$aspect <- 2*pi/360 * dat$asp # convert aspect to radians

##########################################################################
##
## test data
##
tst <- dat[dat$pplot == 22,]

## Test quadrat transformation
tst$x <- center_quadrats(tst$bqudx)
tst$y <- center_quadrats(tst$bqudy)

## Check its the same as original
image(as.matrix(table(tst$x,tst$y)), main = "transformed")
windows()
image(as.matrix(table(tst$bqudx, tst$bqudy)), main = "original")
table(tst$y, tst$bqudy)
table(tst$x, tst$bqudx)

##########################################################################
##
## test z-values
##

## Calculate a z-value for every quadrat using simulated data
## Y-axis points NE, so with a slope aspect of 315 degrees, the slope should be directly up the
##  y-axis
showslope(33, 225)

## Slope in the N direction, thetaS = 360 degrees (or 0 degrees, either works)
showslope(33, 360)

## Plot z-values for test plot
plotzvals(tst, slope = "slope", aspect = "aspect")

aspect <- unique(tst$asp)
slope <- unique(tst$slope8687)

z <- apply(tst[,c("x","y","slope","aspect")], 1, function(x) {
    p = c(as.numeric(x[["x"]]), as.numeric(x[["y"]])) # create (x,y) point
    slo = unique(as.numeric(x[["slope"]]))
    asp = unique(as.numeric(x[["aspect"]]))
    z <- zval(p, slo, asp)
})

## plot with horizontal plane as well
tt <- rbind(tst, tst)
tt$z <- rep(z, 2)
tt <- tt[!is.na(tt$z), ]
tt$grp <- factor(rep(c("A","B"), each = length(z)))
wireframe(z ~ x*y, data = tt[!is.na(tt$z),], group = grp)



