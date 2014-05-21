## Add z-values for all trees on Moosilauke
## Origin is at the center of the plot
dat <- read.csv("~/work/data/data/moose-wide.csv")
dat$slope <- 2*pi/360 * dat$slope8687 # convert slope to radians
dat$theta <- 2*pi/360 * dat$asp # convert aspect to radians

##########################################################################
##
## test data, data from plot 4, quadrats > 0 and <= 10 (removing outliers)
##
tst <- dat[dat$pplot == 4 & dat$bqudy > 0 & dat$bqudy <= 10 &
           !is.na(dat$bqudy) & !is.na(dat$bqudx) & dat$bqudx > 0 &
           dat$bqudx <= 10, ]
newx <- newy <- seq(-4.5, 4.5, 1)
xx <- tst[!is.na(tst$bqudx) & tst$bqudx > 0 & tst$bqudx <= 10, ]$bqudx
yy <- tst[!is.na(tst$bqudy) & tst$bqudy > 0 & tst$bqudy <= 10, ]$bqudy
x <- newx[xx]
y <- newy[yy]

## Check its the same as original
image(as.matrix(table(x,y)), main = "transformed")
windows()
image(as.matrix(table(tst$bqudx, tst$bqudy)), main = "original")
table(y, tst$bqudy)
table(x, tst$bqudx)

##########################################################################
##
##  compute z-values
##  Angles are in radians
##  thetaS is slope angle, thetaA is slope aspect, p is a point(quadx, quady),
##   thetaB is the angle between thetaA projected onto the 2d plane of the plot
##   and the vector from p through the center of the plot
##
## Note: north if offset from the y-axis by +pi/4
##
zval <- function(p, thetaS, thetaA) {
    offset = pi/4 # North offest from y-axis
    r = sqrt(p[1]^2 + p[2]^2) # distance from p to origin
    a = c(sin(thetaA + offset), cos(thetaA + offset)) # unit vector along slope aspect
    ap <- sum(a * p) # vector product of a and p
    frac <- round(ap/r, 15) # acos will return NA due to precision errors if frac slightly > 1
    thetaB = acos( frac ) # angle between a and p
    sign = ifelse(thetaB < 0, -1, 1) # sign of z-value
    r * cos(thetaB) * tan(thetaS)
}

## Test zvalues
showslope <- function(slope, aspect) {
    require(lattice)
    coords = expand.grid(x = seq(-4.5, 4.5, 1), y = seq(-4.5, 4.5, 1))
    slo = slope * (2*pi/360)
    asp = aspect * (2*pi/360)
    z <- apply(coords, 1, function(x) {
        zval(c(x[["x"]], x[["y"]]), slo, asp)
    })
    coords = rbind(coords, coords)
    coords$z <- c(rep(0, length(z)), z)
    coords$plane <- factor(rep(c("Horizontal", "Inclined"), each = length(z)))

    ## plot
    mycolors.trans = rgb(c(255,0,0), c(0,255,0), c(0,0,255), alpha = 70,
    maxColorValue = 255)

    mycolors = rgb(c(255,0,0), c(0,255,0), c(0,0,255), maxColorValue = 255)

    wireframe(z ~ x*y, data = coords, group = plane, col.groups = mycolors.trans,
              scales = list(arrows = FALSE, col = "black", font = 10),
              key = list(text=list(c("Horizontal", "Inclined"), col = mycolors),
              lines = list(lty = c(1, 1, 1), col = mycolors)),
              par.settings = list(axis.line = list(col = "transparent")),
              zlim = c(-8,8),)
}

## Y-axis points NE, so with a slope aspect of 315 degrees, the slope should be directly up the
##  y-axis
showslope(33, 315)

## Slope in the N direction, thetaS = 360 degrees (or 0 degrees, either works)
showslope(33, 360)

