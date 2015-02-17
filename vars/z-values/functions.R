##########################################################################
##
## Functions used to compute and test z-values
##
##########################################################################
source("~/work/functions/functions-coordinates.R")

################################################################################
##
##                          Using rotation matrices
##
################################################################################
## ps: matrix of homogenous points (x, y, z, 1)
## theta_s: slope angle (radians)
## theta_a: aspect angle (radians)
## x_offset: counter-clockwise offset of true north from +x-axis
zvals <- function(ps, theta_a, theta_s, x_offset = -45, degrees = TRUE) {
    if (degrees) {
        theta_a <- theta_a * pi/180
        theta_s <- theta_s * pi/180
        x_offset <- x_offset * pi/180
    }
    theta_a <- pi - (theta_a + x_offset)             # aspect angle -> DOWN slope
    rmat <- rz(theta_a)                         # rotation matrix around the z-axis
    A <- t( rmat %*% t(ps) )[, 1:2]         # rotated points
    z <- A[,1] * tan(theta_s)                   # x-value is distance down/up slope
    z[abs(z) < 2e-13] <- 0
    return( z )
}

## dat <- read.csv("~/work/data/moose/moose-wide.csv")
## tst <- dat[dat$pplot == 22, ]
## theta_a <- unique(tst$asp)
## theta_s <- unique(tst$slope)
## dims <- nrow(tst)
## ps <- matrix(c(tst$x, tst$y, rep(0, dims), rep(1, dims)), ncol = 4)  # points (homogenous)
## z <- zvals(ps, theta_a, theta_s)

################################################################################
##
##  Wrapper for zval to compute z-values for vector of points
## - Angles are in radians
## - theta_S: is slope angle
## - theta_A: is slope aspect
## - p: points (ie. c(quadx, quady))
## - theta_B: the angle between theta_A projected onto the 2d plane of the plot
##   and the vector from p through the center of the plot
##
################################################################################
## zvals <- function(ps, theta_S, theta_A) {
##     if (length(theta_S) == 1 & length(theta_A) == 1)
##         return ( sapply(1:nrow(ps), FUN = function(i)
##             zval(p=ps[i,], theta_S = theta_S, theta_A = theta_A) ))
##     sapply(1:nrow(ps), FUN = function(i) zval(p=ps[i,], theta_S = theta_S[i], theta_A = theta_A[i]) )
## }

################################################################################
##
##  Compute z-values for a single point 
## - Angles are in radians
## - theta_S: is slope angle
## - theta_A: is slope aspect
## - p: point (ie. c(quadx, quady))
## - theta_B: the angle between theta_A projected onto the 2d plane of the plot
##   and the vector from p through the center of the plot
##
## Note: north if offset from the y-axis by +5pi/4
##
################################################################################
zval <- function(p, theta_S, theta_A) {
    if (is.na(p[1])|| is.na(p[2]) || is.na(theta_S) || is.na(theta_A))
        return ( NA )
    if (p[1] == 0 && p[2] == 0) return ( 0 )
    offset = 5*pi/4                                        # North offest from y-axis
    offset = offset + pi                                   # correct for slope aiming downhill
    r = sqrt(p[1]^2 + p[2]^2)                             # distance from p to origin
    a = c(sin(theta_A + offset), cos(theta_A + offset))           # unit vector along slope aspect
    ap <- sum(a * p)                                      # vector product of a and p
    frac <- round(ap/r, 15)                               # acos will return NA due to precision errors
    theta_B = acos( frac )                                    # angle between a and p
    sign = ifelse(theta_B < 0, -1, 1)                         # sign of z-value
    r * cos(theta_B) * tan(theta_S)
}


################################################################################
##
##         Test zvalues using simulated data: point in each quadrat
##
## - slope/aspect in degrees
##
################################################################################
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



################################################################################
##
##                         Plot z-values given data
##
## - Adds a horizontal plane as reference (i.e. in the plane of the horizon)
## - Provide names of slope, aspect, x, y columns if different than default
## - slope/aspect should be in radians
## - x,y should be centered on origin
## - Specify type of "scatter" or "lattice" for different graphical output
##
################################################################################
plotzvals <- function(dat, slope = "slope", aspect = "aspect",
                      xcol = "x", ycol = "y", type = "scatter") {
    slo = unique(dat[,slope])
    asp = unique(dat[,aspect])
    x = dat[,xcol]
    y = dat[,ycol]
    samp = unique(dat[!is.na(x) & !is.na(y), c("x","y")])
    samp$z = apply(samp, 1, function(x) {
        zval(c(as.numeric(x[["x"]]), as.numeric(x[["y"]])), slo, asp)
    })

    if (type == "scatter") {
        ## 3d scatterplot
        require(scatterplot3d)
        rounded <- round(abs(samp$z + min(samp$z)))    # create a color palette
        colfunc <- colorRampPalette(c("light blue","dark blue"))
        mypalette <- colfunc(length(unique(rounded)))
        cols <- mypalette[rounded+1]
        s3d <- scatterplot3d(samp, type = "p", color = cols,
                             angle = 55, scale.y = 0.7, pch = 16)
        my.lm <- lm(z ~ x+y, data = samp)
        s3d$plane3d(my.lm)
        s3d$plane3d(c(0,0,0), col = "light green")
    }

    if (type == "lattice") {
        ## wireframe plot
        require(lattice)
        dev.new()
        mycolors.trans = rgb(c(255,0,0), c(0,255,0), c(0,0,255), alpha = 70,
        maxColorValue = 255)
        mycolors = rgb(c(255,0,0), c(0,255,0), c(0,0,255), maxColorValue = 255)
        wireframe(z ~ x*y, data = samp, col.groups = mycolors.trans,
                  scales = list(arrows = FALSE, col = "black", font = 10),
                  key = list(text=list(c("2D", "3D"), col = mycolors),
                  lines = list(lty = c(1, 1, 1), col = mycolors)),
                  par.settings = list(axis.line = list(col = "transparent")),
                  zlim = c(-8,8),screen = list(x = -80, y = -35, z = -10),)
    }
}


