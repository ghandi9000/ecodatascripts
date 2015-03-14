### fit.R --- 
## Filename: fit.R
## Description: Fit gompertz (only elevation) to abbas
## Author: Noah Peart
## Created: Wed Mar  4 12:52:06 2015 (-0500)
## Last-Updated: Fri Mar 13 20:53:26 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/vars/heights/gompertz/full/model.R")  # model/fit functions
source("~/work/ecodatascripts/vars/heights/prep.R")                 # data prep
source("~/work/ecodatascripts/vars/heights/canopy/load_canopy.R")   # canopy functions
library(dplyr)
library(magrittr)

################################################################################
##
##                                 Run fits
##
################################################################################
## Only 3 trees from 87
base_dir <- "~/work/ecodatascripts/vars/heights/gompertz/full/abba/"
yrs <- c(86, 98, 10)
spec <- c("abba")
can_func <- "can_hts"  # canopy functions defined in canopy directory

for (yr in yrs) {
    dat <- prep_data(dat=pp, yr=yr, spec=spec, can_func=can_func)
    ps <- readRDS(paste0(base_dir, "abba_", yr, ".rds"))
    ps$gamma <- NULL  # decided to kill intercept param

    method <- "SANN" # "Nelder-Mead"
    maxit <- 1e6

    summary(fit <- run_fit(dat, ps, yr, method=method, maxit=maxit))  # SANN first
    summary(fit2 <- run_fit(dat, as.list(coef(fit)), yr))             # the Nelder-Mead

    ## save parameters
    ps <- as.list(coef(fit2))
    saveRDS(ps, file=paste0(base_dir, "abba_", yr, ".rds"))
}

################################################################################
##
##                              Visualize fits
##
################################################################################
## library(rgl)

## ## Points and predicted points
## pred86 <- gompertz(ps86, dbh=abba86[,"DBH86"], elev=abba86[,"ELEV"])
## pred98 <- gompertz(ps98, dbh=abba98[,"DBH98"], elev=abba98[,"ELEV"])
## pred10 <- gompertz(ps10, dbh=abba10[,"DBH10"], elev=abba10[,"ELEV"])

## plot3d(xyz.coords(abba98[,"DBH98"], abba98[,"ELEV"], pred98),
##        xlab = "DBH", ylab = "Elevation", zlab = "Height",
##        main = "Gompertz allometric model predictions (Elevation only)")
## points3d(xyz.coords(abba86[,"DBH86"], abba86[,"ELEV"], pred86), col = "red")
## points3d(xyz.coords(abba10[,"DBH10"], abba10[,"ELEV"], pred10), col = "brown")

## ## Observed data
## points3d(xyz.coords(abba98[,"DBH98"], abba98[,"ELEV"], abba98[,"HTTCR98"]), col = "blue")
## points3d(xyz.coords(abba86[,"DBH86"], abba86[,"ELEV"], abba86[,"HTTCR86"]), col = "green")
## points3d(xyz.coords(abba10[,"DBH10"], abba10[,"ELEV"], abba10[,"HTTCR10"]), col = "purple")


## ## residuals
## dat <- abba98
## resids <- dat$httcr98 - preds
## plot3d(xyz.coords(dat$dbh98, dat$elev, resids),
##        xlab = "dbh98", ylab = "elev", zlab = "residuals",
##        main = "Gompertz allometric model residuals")
## planes3d(c(0,0,1), alpha = 0.2, col = "red")

## ## Surface
## x <- with(dat, seq(min(dbh98), max(dbh98), length=50))
## y <- with(dat, seq(min(elev), max(elev), length=20))
## z <- outer(x, y, gompertz, ps=ps)
## surface3d(x, y, z, col = "green", alpha = 0.5, front = "line", back = "line")
## points3d(xyz.coords(dat$dbh98, dat$elev, dat$httcr98), col = "red")
## axes3d()
## decorate3d(xlab = "dbh98", ylab="elev", zlab="height98", main = "Predicted surface, observed points")
