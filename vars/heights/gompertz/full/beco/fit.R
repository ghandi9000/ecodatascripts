### fit.R --- 
## Filename: fit.R
## Description: BECO fits
## Author: Noah Peart
## Created: Thu Mar 12 17:58:16 2015 (-0400)
## Last-Updated: Fri Mar 13 21:51:20 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/vars/heights/gompertz/full/model.R")  # model/fit functions
source("~/work/ecodatascripts/vars/heights/canopy/load_canopy.R")   # canopy functions
library(dplyr)
library(magrittr)

################################################################################
##
##                        NOTE: need better model
##
################################################################################

################################################################################
##
##                                 Run fits
##
################################################################################
base_dir <- "~/work/ecodatascripts/vars/heights/gompertz/full/beco/"
yrs <- c(86, 98, 10)
spec <- c("beco")
can_func <- "can_hts"  # canopy functions defined in canopy directory

for (yr in yrs) {
    dat <- prep_data(dat=pp, yr=yr, spec=spec, can_func=can_func)
    ps <- readRDS(paste0(base_dir, "beco_", yr, ".rds"))
    ps$gamma <- NULL  # decided to kill intercept param

    method <- "SANN" # "Nelder-Mead"
    maxit <- 1e6

    summary(fit <- run_fit(dat, ps, yr, method=method, maxit=maxit))  # SANN first
    summary(fit2 <- run_fit(dat, as.list(coef(fit)), yr))             # the Nelder-Mead

    ## save parameters
    ps <- as.list(coef(fit2))
    saveRDS(ps, file=paste0(base_dir, "beco_", yr, ".rds"))
}
