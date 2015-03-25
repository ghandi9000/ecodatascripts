### predict_heights.R --- 
## Filename: predict_heights.R
## Description: Predict missing heights Moosilauke trees (see README.txt for info)
## Author: Noah Peart
## Created: Mon Mar  2 13:37:06 2015 (-0500)
## Last-Updated: Wed Mar 25 19:02:32 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/read/read-moose.R")
source("~/work/ecodatascripts/read/read-transect.R")
source("~/work/ecodatascripts/vars/heights/canopy/load_canopy.R")
library(dplyr)
library(lazyeval)

## Fit directories
basedir <- "~/work/ecodatascripts/vars/heights/"
## gomp_full <- paste0(basedir, "gompertz/full/")
## gomp_elev <- paste0(basedir, "gompertz/elev/")
## gomp_can <- paste0(basedir, "gompertz/elev/")
## negexp_full <- paste0(basedir, "negexp/full/")
## negexp_elev <- paste0(basedir, "negexp/elev/")

################################################################################
##
##                              Permanent Plots
##
################################################################################
## Use local canopy heights (subplot/plot mean of codominant/dominant tree heights)
## canopy function: can_hts
## prep_pp <- function(dat, yr, can_func="can_hts") {
##     require(dplyr)
##     require(lazyeval)
##     ## variable names
##     stat <- paste0("STAT", yr)
##     dbh <- paste0("DBH", yr)
##     ht <- paste0("HTTCR", yr)
##     cols <- c("PPLOT", "SPLOT", "ELEV", "SPEC", "TAG")
    
##     ## Conditions
##     is_alive <- interp(~stat == "ALIVE", stat=as.name(stat))
##     has_dbh <- interp(~!is.na(dbh), dbh=as.name(dbh))
##     res <- dat %>% filter_(~PPLOT > 3, is_alive, has_dbh)

##     ## Canopy (defined by can_func)
##     res$canht <- apply(res, 1, function(x) do.call(can_func, list(row=x, yr=yr)))
##     res[,c(cols, stat, dbh, ht, "canht")]
## }

params_pp <- list(
    ABBA = list(spp = "abba", model = "gompertz", inds = "full", yrs = c(86, 98, 98, 10)),
    PIRU = list(spp = "piru", model = "gompertz", inds = "full", yrs = c(86, 87, 98, 10)),
    ## Betula spp.
    BECO = list(spp = "beco", model = "negexp", inds = "full", yrs = c(86, 98, 98, 10)),
    BEPA = list(spp = "betula", model = "negexp", inds = "full", yrs = c(86, 98, 98, 10)),
    BEAL = list(spp = "betula", model = "negexp", inds = "full", yrs = c(86, 98, 98, 10)),
    ## Maples
    ACSA = list(spp = "maples", model = "gompertz", inds = "full", yrs = c(98, 98, 98, 10)),
    ACPE = list(spp = "maples", model = "gompertz", inds = "full", yrs = c(98, 98, 98, 10)),
    ACSP = list(spp = "maples", model = "gompertz", inds = "full", yrs = c(98, 98, 98, 10)),
    ## Others
    FAGR = list(spp = "hardwoods", model = "gompertz", inds = "full", yrs = c(98, 98, 98, 10)),
    SOAM = list(spp = "soam", model = "gompertz", inds = "full", yrs = c(98, 98, 98, 98)),
    PRPE = list(spp = "soam", model = "gompertz", inds = "full", yrs = c(98, 98, 98, 98)),
    PRVI = list(spp = "soam", model = "gompertz", inds = "full", yrs = c(98, 98, 98, 98))
    ## UNID and unlabeled?
)

fit_pp <- function(dat, pars, basedir, yrs=c(86, 87, 98, 10), can_func="can_hts") {
    dat[, paste0("htpred", yrs)] <- NA
    dat[, paste0("canht", yrs)] <- NA

    ## This part is slow, should add canht more efficiently
    for (yr in yrs) dat[, paste0("canht", yr)] <- apply(dat, 1, function(x)
        do.call(can_func, list(row=x, yr=yr)))  # add canopy heights

    for (spp in levels(dat$SPEC)) {
        if (spp %in% names(pars)) {  # ignore unidentified species
            cat(paste("Fitting", spp, '\n'))
            moddir <- paste0(basedir, pars[[spp]]$model, "/", pars[[spp]]$inds, "/")
            source(paste0(moddir, "model.R"))                        # source the approriate model
            for (i in 1:length(yrs)) {
                ps <- readRDS(paste0(moddir, tolower(pars[[spp]]$spp), "/", tolower(pars[[spp]]$spp),
                                     "_", pars[[spp]]$yrs[i], ".rds"))  # read fit parameters
                inds <- dat$SPEC == spp
                dat[inds, paste0("htpred", yrs[i])] <-
                    do.call(pars[[spp]]$model,
                            list(ps = ps,
                                 dbh = dat[inds, paste0("DBH", yrs[i])],
                                 elev = dat[inds, "ELEV"],
                                 canht = dat[inds, paste0("canht", yrs[i])]))
            }
        }
    }
    dat
}

################################################################################
##
##                                   Check
##
################################################################################
tst <- fit_pp(pp, params_pp, basedir)

## Heights not fit: check species
missed <- tst[tst$PPLOT > 3 & !is.na(tst$DBH87) & is.na(tst$htpred87), ]
table(missed$SPEC) # should be just UNID and/or ""

## Look at HT vs. DBH for species
specs <- names(params_pp)
yr <- 98

stat <- paste0("STAT", yr)
dbh <- paste0("DBH", yr)
ht <- paste0("HTTCR", yr)
pred <- paste0("htpred", yr)

par(mfrow=c(4, 3))
for (spp in specs) {
    inds <- tst$SPEC == toupper(spp) & tst[[stat]] == "ALIVE" & tst$PPLOT > 3
    if (sum(inds) == 0) cat(paste("No data for", spp, "\n"))
    else {
        ylims = c(min(c(tst[inds, ht], tst[inds, pred]), na.rm=TRUE) - 1,
        max(c(tst[inds, ht], tst[inds, pred]), na.rm = TRUE) + 1)
        plot(tst[inds, dbh], tst[inds, ht], ylim=ylims, main = paste("Species:", spp, ", Year:", yr))
        points(tst[inds, dbh], tst[inds, pred], col="red", pch=16)
    }
}

################################################################################
##
##                                 Transects
##
################################################################################
## Remember: HH fit with canht only models, rest fit with elev only models
## For low elevation, use exact same models, execpt only elevation as inds, and
##  use 98, 10 fits 
## For HH: use canht only gompertz model, canopy function: "can_hh"
params_tp_low <- lapply(params_pp, function(x) {
    res <- x
    res$inds = "elev"
    res$yrs <- c(98, 98, 98, 10, 10)
    res
})

## Some additional species groupings in transect
params_tp_low[["BESPP"]] <- params_tp_low[["BEAL"]]
params_tp_low[["UNCO"]] <- params_tp_low[["ABBA"]]  # unidentified coniferous, treat as ABBA

fit_tp_low <- function(dat, pars, basedir, yrs=c(87, 98, 99, 10, 11)) {
    ## spec <- toupper(spec)
    dat[, paste0("htpred", yrs)] <- NA

    for (spp in levels(dat$SPEC)) {
        if (spp %in% names(pars)) {  # ignore unidentified species
            cat(paste("Fitting", spp, '\n'))
            moddir <- paste0(basedir, pars[[spp]]$model, "/", pars[[spp]]$inds, "/")
            source(paste0(moddir, "model.R"))                        # source the approriate model
            for (i in 1:length(yrs)) {
                ps <- readRDS(paste0(moddir, tolower(pars[[spp]]$spp), "/", tolower(pars[[spp]]$spp),
                                     "_", pars[[spp]]$yrs[i], ".rds"))  # read fit parameters
                inds <- dat$SPEC == spp
                dat[inds, paste0("htpred", yrs[i])] <-
                    do.call(pars[[spp]]$model,
                            list(ps = ps,
                                 dbh = dat[inds, paste0("DBH", yrs[i])],
                                 elev = dat[inds, "ELEV"]))
            }
        }
    }
    dat
}

