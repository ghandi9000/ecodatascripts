### fit.R --- 
## Filename: fit.R
## Description: ACSA, ACPE, ACSP
## Author: Noah Peart
## Created: Thu Mar 12 19:12:37 2015 (-0400)
## Last-Updated: Thu Mar 12 19:18:23 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/read/read-moose.R")
source("~/work/ecodatascripts/vars/heights/gompertz/elev/model.R")
library(dplyr)
library(magrittr)

spec <- grep("^AC", unique(pp$SPEC), value=T)
setwd("~/work/ecodatascripts/vars/heights/gompertz/elev/maples")  # for pars

################################################################################
##
##                                   2010
##
################################################################################
maples10 <- pp %>% filter(STAT10 == "ALIVE", !is.na(DBH10), SPEC %in% toupper(spec),
                        !is.na(HTTCR10)) %>%
                            select(PPLOT, ELEVCL, ASPCL, ends_with("10"), ELEV, SPEC)
ps10 <- readRDS(paste0("./maples_10.rds"))  # fit parameters

################################################################################
##
##                                   1998
##
################################################################################
## Live MAPLESs from 1998
maples98 <- pp %>% filter(STAT98 == "ALIVE", !is.na(DBH98), SPEC %in% toupper(spec),
                        !is.na(HTTCR98)) %>%
                            select(PPLOT, ELEVCL, ASPCL, ends_with("98"), ELEV, SPEC)
ps98 <- readRDS(paste0("./maples_98.rds"))

################################################################################
##
##                                   1986
##
################################################################################
## only 5 trees!
## maples86 <- pp %>% filter(STAT86 == "ALIVE", !is.na(DBH86), SPEC %in% toupper(spec),
##                         !is.na(HTTCR86)) %>%
##                             select(PPLOT, ELEVCL, ASPCL, ends_with("86"), ELEV, SPEC)
## ps86 <- readRDS("./maples_86.rds")

################################################################################
##
##                                   1987
##
################################################################################
## No height measurements
## maples87 <- pp %>% filter(STAT87 == "ALIVE", !is.na(DBH87), SPEC %in% toupper(spec),
##                         !is.na(HTTCR87)) %>%
##                             select(PPLOT, ELEVCL, ASPCL, ends_with("87"), ELEV)

################################################################################
##
##                                 Run fits
##
################################################################################
yrs <- c(98, 10)
for (yr in yrs) {
    dat <- get(paste0("maples",yr))
    ps <- get(paste0("ps",yr))
    ps$gamma <- NULL  # decided to kill intercept param

    method <- "SANN" # "Nelder-Mead"
    maxit <- 1e6

    fit <- run_fit(dat, ps, yr, method=method, maxit=maxit)           # SANN first
    print("SANN fit summary")
    print(summary(fit))
    
    summary(fit2 <- run_fit(dat, as.list(coef(fit)), yr))             # the Nelder-Mead
    print("Nelder-Mead fit summary")
    print(summary(fit2))
    
    ## save parameters
    ps <- as.list(coef(fit))
    saveRDS(ps, file=paste0("~/work/ecodatascripts/vars/heights/gompertz/elev/maples/maples_", yr, ".rds"))
}
