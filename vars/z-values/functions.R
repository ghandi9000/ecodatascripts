##########################################################################
##
## Functions used to compute and test z-values
##
##########################################################################
##
##  compute z-values
##  Angles are in radians
##  thetaS is slope angle, thetaA is slope aspect, p is a point(quadx, quady),
##   thetaB is the angle between thetaA projected onto the 2d plane of the plot
##   and the vector from p through the center of the plot
##
## Note: north if offset from the y-axis by -pi/4
##
zval <- function(p, thetaS, thetaA) {
    offset = -pi/4 # North offest from y-axis
    r = sqrt(p[1]^2 + p[2]^2) # distance from p to origin
    a = c(sin(thetaA + offset), cos(thetaA + offset)) # unit vector along slope aspect
    ap <- sum(a * p) # vector product of a and p
    frac <- round(ap/r, 15) # acos will return NA due to precision errors if frac slightly > 1
    thetaB = acos( frac ) # angle between a and p
    sign = ifelse(thetaB < 0, -1, 1) # sign of z-value
    r * cos(thetaB) * tan(2*pi - thetaS)
}


## Test zvalues using simulated data: point in each quadrat
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


## Plot z-values given data, and add a horizontal plane as reference,
##  Provide names of slope, aspect, x, y columns if different than default
##  Specify type of "scatter" or "lattice"
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
        rounded <- round(abs(samp$z + min(samp$z)))    ## create a color palette
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


