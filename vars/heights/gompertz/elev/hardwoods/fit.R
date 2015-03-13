### fit.R --- 
## Filename: fit.R
## Description: maples and fagr
## Author: Noah Peart
## Created: Thu Mar 12 18:15:54 2015 (-0400)
## Last-Updated: Thu Mar 12 20:26:06 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/read/read-moose.R")
source("~/work/ecodatascripts/vars/heights/gompertz/elev/model.R")
library(dplyr)
library(magrittr)

spec <- grep("^AC|FA", unique(pp$SPEC), value=T)
setwd("~/work/ecodatascripts/vars/heights/gompertz/elev/hardwoods")  # for pars

################################################################################
##
##                                   2010
##
################################################################################
hardwoods10 <- pp %>% filter(STAT10 == "ALIVE", !is.na(DBH10), SPEC %in% toupper(spec),
                        !is.na(HTTCR10)) %>%
                            select(PPLOT, ELEVCL, ASPCL, ends_with("10"), ELEV, SPEC)
ps10 <- readRDS(paste0("./hardwoods_10.rds"))  # fit parameters

################################################################################
##
##                                   1998
##
################################################################################
## Live HARDWOODSs from 1998
hardwoods98 <- pp %>% filter(STAT98 == "ALIVE", !is.na(DBH98), SPEC %in% toupper(spec),
                        !is.na(HTTCR98)) %>%
                            select(PPLOT, ELEVCL, ASPCL, ends_with("98"), ELEV, SPEC)
ps98 <- readRDS(paste0("./hardwoods_98.rds"))

################################################################################
##
##                                   1986
##
################################################################################
## only 5 trees
hardwoods86 <- pp %>% filter(STAT86 == "ALIVE", !is.na(DBH86), SPEC %in% toupper(spec),
                        !is.na(HTTCR86)) %>%
                            select(PPLOT, ELEVCL, ASPCL, ends_with("86"), ELEV, SPEC)

################################################################################
##
##                                   1987
##
################################################################################
## No height measurements
hardwoods87 <- pp %>% filter(STAT87 == "ALIVE", !is.na(DBH87), SPEC %in% toupper(spec),
                        !is.na(HTTCR87)) %>%
                            select(PPLOT, ELEVCL, ASPCL, ends_with("87"), ELEV)

################################################################################
##
##                                 Run fits
##
################################################################################
yrs <- c(98, 10)
for (yr in yrs) {
    dat <- get(paste0("hardwoods",yr))
    ps <- get(paste0("ps",yr))
    ps$gamma <- NULL  # decided to kill intercept param

    method <- "SANN" # "Nelder-Mead"
    maxit <- 1e6

    summary(fit <- run_fit(dat, ps, yr, method=method, maxit=maxit))  # SANN first
    summary(fit2 <- run_fit(dat, as.list(coef(fit)), yr))             # the Nelder-Mead

    ## save parameters
    ps <- as.list(coef(fit))
    saveRDS(ps, file=paste0("~/work/ecodatascripts/vars/heights/gompertz/elev/hardwoods/hardwoods_", yr, ".rds"))
}
