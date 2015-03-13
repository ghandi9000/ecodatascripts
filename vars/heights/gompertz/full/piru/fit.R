### fit.R --- 
## Filename: fit.R
## Description: Gompertz allometric fit for PIRU (no canopy)
## Author: Noah Peart
## Created: Wed Mar 11 20:30:57 2015 (-0400)
## Last-Updated: Thu Mar 12 18:57:44 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/read/read-moose.R")
source("~/work/ecodatascripts/vars/heights/gompertz/elev/model.R")
library(dplyr)
library(magrittr)

################################################################################
##
##                                   2010
##
################################################################################
piru10 <- pp %>% filter(STAT10 == "ALIVE", !is.na(DBH10), SPEC == "PIRU", !is.na(HTTCR10)) %>%
    select(PPLOT, ELEVCL, ASPCL, ends_with("10"), ELEV)
ps10 <- readRDS("./piru_10.rds")  # fit parameters

################################################################################
##
##                                   1998
##
################################################################################
## Live PIRUs from 1998
piru98 <- pp %>% filter(STAT98 == "ALIVE", !is.na(DBH98), SPEC == "PIRU", !is.na(HTTCR98)) %>%
    select(PPLOT, ELEVCL, ASPCL, ends_with("98"), ELEV)
ps98 <- readRDS("./piru_98.rds")

################################################################################
##
##                                   1986
##
################################################################################
## Use heights measured in 87 for 86 dbhs?
piru86 <- pp %>% filter(STAT86 == "ALIVE", !is.na(DBH86), SPEC == "PIRU",
                        !is.na(HTTCR86)) %>%
                            select(PPLOT, ELEVCL, ASPCL, ends_with("86"), ELEV)
ps86 <- readRDS("./piru_86.rds")

################################################################################
##
##                                   1987
##
################################################################################
## not enough pirus in 1987 with height and dbh
piru87 <- pp %>% filter(STAT87 == "ALIVE", !is.na(DBH87), SPEC == "PIRU",
                        !is.na(HTTCR87)) %>%
                            select(PPLOT, ELEVCL, ASPCL, ends_with("87"), ELEV)
ps87 <- readRDS("./piru_87.rds")

################################################################################
##
##                                 Run fits
##
################################################################################
yrs <- c(86, 87, 98, 10)
for (yr in yrs) {
    dat <- get(paste0("piru", yr))
    ps <- get(paste0("ps", yr))
    ps$gamma <- NULL  # decided to kill intercept param

    method <- "SANN" # "Nelder-Mead"
    maxit <- 1e6

    summary(fit <- run_fit(dat, ps, yr, method=method, maxit=maxit))  # SANN first
    summary(fit2 <- run_fit(dat, as.list(coef(fit)), yr))             # the Nelder-Mead

    ## save parameters
    ps <- as.list(coef(fit))
    saveRDS(ps, file=paste0("~/work/ecodatascripts/vars/heights/gompertz/elev/piru/piru_", yr, ".rds"))
}
