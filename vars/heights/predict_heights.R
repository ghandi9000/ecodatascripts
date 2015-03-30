### predict_heights.R --- 
## Filename: predict_heights.R
## Description: Predict missing heights Moosilauke trees (see README.txt for info)
## Author: Noah Peart
## Created: Mon Mar  2 13:37:06 2015 (-0500)
## Last-Updated: Mon Mar 30 00:13:44 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/read/read-moose.R")                  # permanent plot data
source("~/work/ecodatascripts/read/read-transect.R")               # transect data
source("~/work/ecodatascripts/vars/heights/canopy/load_canopy.R")  # canopy functions/data
library(dplyr)
library(lazyeval)

## Base directory for height predictions
basedir <- "~/work/ecodatascripts/vars/heights/"

################################################################################
##
##                              Permanent Plots
##
################################################################################
## Use local canopy heights (subplot/plot mean of codominant/dominant tree heights)
## canopy function: can_pp
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
    ## UNID and unlabeled? -- currently ignored
)

fit_pp <- function(dat, pars, basedir, yrs=c(86, 87, 98, 10), can_func="can_pp") {
    require(dplyr)
    dat[, paste0("htpred", yrs)] <- NA

    ## Add canopy heights
    dat <- dat %>% group_by(PPLOT, SPLOT) %>%
        do(do.call(can_func, list(dat=., plot=unique(.$PPLOT), splot=unique(.$SPLOT), yrs=yrs)))

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
        else
            cat(paste("Skipping", spp, "\n"))
    }
    data.frame(dat)
}

################################################################################
##
##                                   Check
##
################################################################################
tst <- fit_pp(pp, params_pp, basedir)

## Heights not fit: check species
missed <- tst[tst$PPLOT > 3 & !is.na(tst$DBH86) & is.na(tst$htpred86), ]
table(missed$SPEC) # should be just UNID and/or ""

## Look at HT vs. DBH for species
specs <- names(params_pp)
yr <- 86

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
        else
            cat(paste("Skipping", spp))
    }
    dat
}

################################################################################
##
##                                   Check
##
################################################################################
tst <- fit_tp_low(tp, params_tp_low, basedir)

## Heights not fit: check species
yrs <- c(87, 98, 99, 10, 11)
for (yr in yrs) {
    htcol <- paste0("htpred", yr)
    dbhcol <- paste0("DBH", yr)
    missed <- tst[!is.na(tst[[dbhcol]]) & is.na(tst[[htcol]]), ]
    cat(paste("Year", yr, "\n"))
    print(table(missed$SPEC)) # should be just UNID and/or ""
    cat("\n")
}

## Look at HT vs. DBH for species
specs <- names(params_tp_low)
yr <- 99

stat <- paste0("STAT", yr)
dbh <- paste0("DBH", yr)
ht <- paste0("HT", yr)
pred <- paste0("htpred", yr)

par(mfrow=c(4, 4))
for (spp in specs) {
    inds <- tst$SPEC == toupper(spp) & tst[[stat]] == "ALIVE"
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
##                               HH Transects
##
################################################################################
params_tp_hh <- list(
    ABBA = list(spp = "abba", model = "gompertz", inds = "can", yrs = c(99, 99, 99, 11, 11)),
    PIRU = list(spp = "abba", model = "gompertz", inds = "can", yrs = c(99, 99, 99, 11, 11)),
    BECO = list(spp = "beco", model = "gompertz", inds = "can", yrs = c(99, 99, 99, 11, 11))
)

fit_tp_hh <- function(dat, pars, basedir, yrs=c(87, 98, 99, 10, 11), can_func="can_hh_add") {
    dat[, paste0("HHhtpred", yrs)] <- NA

    ## Add canopy heights
    dat <- dat %>% group_by(TRAN, TPLOT) %>%
        do(do.call(can_func, list(dat=., tran=unique(as.character(.$TRAN)), tplot=unique(.$TPLOT), yrs=yrs)))

    for (spp in levels(dat$SPEC)) {
        if (spp %in% names(pars)) {  # ignore unidentified species
            cat(paste("Fitting", spp, '\n'))
            moddir <- paste0(basedir, pars[[spp]]$model, "/", pars[[spp]]$inds, "/")
            source(paste0(moddir, "model.R"))                        # source the approriate model
            for (i in 1:length(yrs)) {
                ps <- readRDS(paste0(moddir, tolower(pars[[spp]]$spp), "/", tolower(pars[[spp]]$spp),
                                     "_", pars[[spp]]$yrs[i], ".rds"))  # read fit parameters
                inds <- dat$SPEC == spp & dat$ELEVCL == "HH"
                dat[inds, paste0("HHhtpred", yrs[i])] <-
                    do.call(pars[[spp]]$model,
                            list(ps = ps,
                                 dbh = dat[inds, paste0("DBH", yrs[i])],
                                 canht = dat[inds, paste0("canht", yrs[i])]))
            }
        }
        else
            cat(paste("Skipping", spp, "\n"))
    }
    data.frame(dat)
}

################################################################################
##
##                                   Check
##
################################################################################
tst <- fit_tp_hh(tp, params_tp_hh, basedir)

## Heights not fit: check species
yrs <- c(87, 98, 99, 10, 11)
for (yr in yrs) {
    htcol <- paste0("HHhtpred", yr)
    dbhcol <- paste0("DBH", yr)
    missed <- tst[!is.na(tst[[dbhcol]]) & is.na(tst[[htcol]]) & tst$ELEVCL=="HH" &
                    !(tst$TRAN == "E335" & tst$TPLOT == 16) & !grepl("^S", tst$TRAN), ]
    cat(paste("Year", yr, "\n"))
    print(table(missed$SPEC)) # should be just UNID and/or ""
    cat("\n")
}

## Look at HT vs. DBH for species
specs <- names(params_tp_hh)
yr <- 99

stat <- paste0("STAT", yr)
dbh <- paste0("DBH", yr)
ht <- paste0("HT", yr)
pred <- paste0("HHhtpred", yr)

par(mfrow=c(2, 2))
for (spp in specs) {
    inds <- tst$SPEC == toupper(spp) & tst[[stat]] == "ALIVE" & tst$ELEVCL == "HH"
    if (sum(inds) == 0 || spp == "PIRU") cat(paste("No data for", spp, "\n"))
    else {
        ylims = c(min(c(tst[inds, ht], tst[inds, pred]), na.rm=TRUE) - 1,
        max(c(tst[inds, ht], tst[inds, pred]), na.rm = TRUE) + 1)
        plot(tst[inds, dbh], tst[inds, ht], ylim=ylims, main = paste("Species:", spp, ", Year:", yr))
        points(tst[inds, dbh], tst[inds, pred], col="red", pch=16)
    }
}
