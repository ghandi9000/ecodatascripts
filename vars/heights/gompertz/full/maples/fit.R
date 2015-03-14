### fit.R --- 
## Filename: fit.R
## Description: ACSA, ACPE, ACSP
## Author: Noah Peart
## Created: Thu Mar 12 19:12:37 2015 (-0400)
## Last-Updated: Fri Mar 13 20:58:46 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/vars/heights/gompertz/full/model.R")  # model/fit functions
source("~/work/ecodatascripts/vars/heights/canopy/load_canopy.R")   # canopy functions
library(dplyr)
library(magrittr)

################################################################################
##
##                                 Run fits
##
################################################################################
## Can only fit for 98 and 10
base_dir <- "~/work/ecodatascripts/vars/heights/gompertz/full/maples/"
yrs <- c(98, 10)
spec <- grep("^AC", unique(pp$SPEC), value=T)
can_func <- "can_hts"  # canopy functions defined in canopy directory

for (yr in yrs) {
    dat <- prep_data(dat=pp, yr=yr, spec=spec, can_func=can_func)
    ps <- readRDS(paste0(base_dir, "maples_", yr, ".rds"))
    ps$gamma <- NULL  # decided to kill intercept param

    method <- "SANN" # "Nelder-Mead"
    maxit <- 1e6

    summary(fit <- run_fit(dat, ps, yr, method=method, maxit=maxit))  # SANN first
    summary(fit2 <- run_fit(dat, as.list(coef(fit)), yr))             # the Nelder-Mead

    ## save parameters
    ps <- as.list(coef(fit2))
    saveRDS(ps, file=paste0(base_dir, "maples_", yr, ".rds"))
}
