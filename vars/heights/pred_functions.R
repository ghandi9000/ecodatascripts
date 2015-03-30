### pred_functions.R --- 
## Filename: pred_functions.R
## Description: Functions for predicting heights for permanent/transect plots
## Author: Noah Peart
## Created: Mon Mar 30 13:47:28 2015 (-0400)
## Last-Updated: Mon Mar 30 14:10:01 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/vars/heights/canopy/load_canopy.R")  # canopy functions/data

## Permanent plots
fit_pp <- function(dat, pars, basedir, yrs=c(86, 87, 98, 10), can_func="can_pp") {
    require(dplyr)
    cat("Fitting Permanent Plots\n")
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

## Transects with no canopy (elevation only model)
fit_tp_low <- function(dat, pars, basedir, yrs=c(87, 98, 99, 10, 11)) {
    cat("Fitting low transects\n")
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
            cat(paste("Skipping", spp, "\n"))
    }
    dat
}

## Transects, HH (model with canopy height no elevation)
fit_tp_hh <- function(dat, pars, basedir, yrs=c(87, 98, 99, 10, 11), can_func="can_hh_add") {
    cat("Fitting HH transects\n")
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

