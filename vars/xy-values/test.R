##########################################################################
##
## Compare original quadrat layout with coordinate transform
##
##########################################################################
source("~/work/ecodatascripts/vars/xy-values/functions.R")

## First possibility, y-axis on N -> E
plot(0:11, 0:11, type = "n", xlab = "X", ylab = "Y",
     main = "Y-axis: N -> E")
addl <- sapply(0.5:10.5, function(i) {
    lines(x = c(i,i), y = c(.5,10.5))
    lines(x = c(.5,10.5), y = c(i,i))
})
text(x = c(0,0,0,5.5,10.5,10.5,10.5,5.5),
     y = c(0,5.5,11,11,11,5.5,0,0),
     labels = c("N","NE","E","SE","S","SW","W","NW"),
     cex = 2, col = "dark blue") # directions
addq <- sapply(1:10, function(i) {
    labs = paste0("(", rep(i,10), ",", 1:10, ")")
    text(rep(i,10), 1:10, labels = labs, cex = 0.75)
})
abline(h = 5.5, v = 5.5, col = "red")

## Second possibility, y-axis on N -> W
plot(0:11, 0:11, type = "n", xlab = "X", ylab = "Y",
     main = "Y-axis: N -> W")
addl <- sapply(0.5:10.5, function(i) {
    lines(x = c(i,i), y = c(.5,10.5))
    lines(x = c(.5,10.5), y = c(i,i))
})
text(x = c(0,0,0,5.5,10.5,10.5,10.5,5.5),
     y = c(0,5.5,11,11,11,5.5,0,0),
     labels = c("N","NW","W","SW","S","SE","E","NE"),
     cex = 2, col = "dark blue") # directions
addq <- sapply(1:10, function(i) {
    labs = paste0("(", rep(i,10), ",", 1:10, ")")
    text(rep(i,10), 1:10, labels = labs, cex = 0.75)
})
abline(h = 5.5, v = 5.5, col = "red")

## Sample data when y-axis is along N->E, make factor for quadrat (N,S,E,W)
samp <- expand.grid(x = 1:10, y = 1:10)
samp$quad <- factor(x = ifelse(samp$x < 5.5 & samp$y < 5.5, "N",
                    ifelse(samp$x < 5.5, "E",
                           ifelse(samp$y < 5.5, "W", "S"))))
plot(samp$x, samp$y, type = "n", xlab = "x", ylab = "y",
     main = "Subplots when y-axis: N -> E")
text(x=samp$x,y=samp$y, label = samp$quad)

## Transform data
samp$xt <- center_quadrats(samp$x)
samp$yt <- center_quadrats(samp$y)

## Plot transformed data
plot(samp$xt, samp$yt, type = "n", main = "Subplot Directions (N->E) after Transform",
     ylab = "Y", xlab = "X")
text(x=samp$xt,y=samp$yt, label = samp$quad)

## Possibilities:
## If Y-axis goes N -> E, then y-axis is NW -> SE on transformed data.
## - In this case, offset of y-axis from north is 135 degrees (3pi/4)
## If Y-axis goes N -> W, then y-axis is NE -> SW on transformed data
## - In this case, offset of y-axis from north is 225 degrees (5pi/4)

## Convert real data
dat <- read.csv("~/work/data/data/moose-wide.csv")
dat$x <- center_quadrats(dat$bqudx)
dat$y <- center_quadrats(dat$bqudy)

## Compare numbers of each
orig <- table(dat$bqudx, dat$bqudy)
trans <- table(dat$x, dat$y)
orig[2:11, 2:11] == trans

