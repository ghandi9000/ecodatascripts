### visualize_fits.R --- 
## Filename: visualize_fits.R
## Description: Some visuals for gompertz fits
## Author: Noah Peart
## Created: Wed Mar 11 20:33:24 2015 (-0400)
## Last-Updated: Tue Mar 17 23:22:00 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/read/read-moose.R")
source("~/work/ecodatascripts/vars/heights/prep.R")  # preps data, adds canhts
## source("~/work/ecodatascripts/vars/heights/gompertz/elev/model.R")
## source("~/work/ecodatascripts/vars/heights/negexp/full/model.R")
source("~/work/ecodatascripts/vars/heights/negexp/elev/model.R")
library(rgl)
library(plyr)
library(dplyr)

## Get predictions for years/species
get_preds <- function(spec, years, modtype="negexp", inds="full") {
    species <- unique(pp$SPEC)
    specs <- list(maples=grep("^AC", species, value = T),           # ACSA, ACPE, ACSP
                  hardwoods=grep("^AC|FA", species, value = T),     # maples + FAGR
                  betula=grep("^BE", species, value = T))           # All betulas
    base_dir <- paste0("~/work/ecodatascripts/vars/heights/", modtype, "/", inds, "/")
    par_dir <- paste0(base_dir, "/", spec, "/")
    keep_cols <- c("SPEC", "ELEV", "canht")
    sppgroup <- spec
    if (spec %in% names(specs)) sppgroup <- specs[[spec]]
    ps <- lapply(years, FUN = function(yr){
        pars <- readRDS(paste0(par_dir, tolower(spec), "_", yr, ".rds"))
        stat <- paste0("STAT", yr)
        dbh <- paste0("DBH", yr)
        ht <- paste0("HTTCR", yr)
        dat <- prep_data(pp, yr=yr, spec=toupper(sppgroup))
        if (inds == "full")
            pred <- do.call(modtype, list(pars, dat[,dbh], dat[,"ELEV"], dat[,"canht"]))
        else
            pred <- do.call(modtype, list(pars, dat[,dbh], dat[,"ELEV"]))
        res <- cbind(dat[,c(dbh, ht, keep_cols)], pred=pred)
        names(res) <- gsub("[[:digit:]]", "", tolower(names(res)))  # don't track yrs here
        attr(res, "ps") <- pars
        attr(res, "yr") <- yr
        attr(res, "mod") <- modtype
        attr(res, "inds") <- inds
        res
    })
    names(ps) <- lapply(years, FUN=function(yr) paste0(spec, yr))
    return( ps )
}

plot_preds <- function(preds) {
    ## Plots predicted points
    require(rgl)
    dat <- preds[[1]]
    cols <- palette()[1:length(preds)]
    modtype <- attr(dat, "mod")  # model type used
    inds <- attr(dat, "inds")  # independent vars
    plot3d(xyz.coords(dat[, "dbh"], dat[, "elev"], dat[, "pred"]),
           xlab = "DBH", ylab = "Elevation", zlab = "Height", col=cols[1],
           main = paste0(modtype, " allometric model predictions (,", inds, ") for ", spec))
    if (length(preds) > 1) {
        for (i in 2:length(preds)) {
            dat <- preds[[i]]
            points3d(xyz.coords(dat[, "dbh"], dat[, "elev"], dat[, "pred"]), col=cols[i])
        }
    }
    yrs <- lapply(preds, FUN = function(x) attr(x, "yr"))
    legend3d("topleft", legend=yrs, text.col=cols)
}

add_pred_lines <- function(preds) {
    cols <- palette()[1:length(preds)]
    for (i in 1:length(preds)) {
        dat <- preds[[i]]
        mod <- as.name(attr(dat, "mod"))
        ps <- attr(dat, "ps")
        x <- seq(0, max(dat[,"dbh"]), length=50) # min(dat[,"dbh"])
        y <- unique(dat[,"elev"])
        z <- outer(x, y, mod, ps=ps)
        for (j in 1:ncol(z)) {
            lines3d(xyz.coords(x, rep(y[j], length(x)), z = z[,j]), col=cols[i], lwd=2)
        }
    }
}

add_observed <- function(preds, col=NULL) {
    require(rgl)
    if(!missing(col)) 
        cols <- palette()[1:length(preds)]
    for (i in 1:length(preds)) {
        dat <- preds[[i]]
        points3d(xyz.coords(dat[, "dbh"], dat[, "elev"], dat[, "httcr"]), col=cols[i], pch = 16, pwd=2)
    }
}

dat <- preds[[1]]
points3d(xyz.coords(dat[, "dbh"], dat[, "elev"], dat[, "httcr"]), col="red", pch = 16, pwd=2)
################################################################################
##
##                                 Visualize
##
################################################################################
spec <- "hardwoods"
years <- c(98)
preds <- get_preds(spec, years, modtype = "gompertz", inds = "full")
plot_preds(preds)
add_pred_lines(preds)
add_observed(preds, col = "red")


## residuals
dat <- abba98
resids <- dat$httcr98 - preds
plot3d(xyz.coords(dat$dbh98, dat$elev, resids),
       xlab = "dbh98", ylab = "elev", zlab = "residuals",
       main = "Gompertz allometric model residuals")
planes3d(c(0,0,1), alpha = 0.2, col = "red")

## Surface
x <- with(dat, seq(min(dbh98), max(dbh98), length=50))
y <- with(dat, seq(min(elev), max(elev), length=20))
z <- outer(x, y, gompertz, ps=ps)
surface3d(x, y, z, col = "green", alpha = 0.5, front = "line", back = "line")
points3d(xyz.coords(dat$dbh98, dat$elev, dat$httcr98), col = "red")
axes3d()
decorate3d(xlab = "dbh98", ylab="elev", zlab="height98", main = "Predicted surface, observed points")

