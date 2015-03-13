### fit.R --- 
## Filename: fit.R
## Description: BECO fits
## Author: Noah Peart
## Created: Thu Mar 12 17:58:16 2015 (-0400)
## Last-Updated: Thu Mar 12 18:55:48 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/read/read-moose.R")
source("~/work/ecodatascripts/vars/heights/gompertz/elev/model.R")
library(dplyr)
library(magrittr)

spec <- "beco"

################################################################################
##
##                                   2010
##
################################################################################
beco10 <- pp %>% filter(STAT10 == "ALIVE", !is.na(DBH10), SPEC == toupper(spec),
                        !is.na(HTTCR10)) %>%
                            select(PPLOT, ELEVCL, ASPCL, ends_with("10"), ELEV)
ps10 <- readRDS(paste0("./", spec, "_10.rds"))  # fit parameters

################################################################################
##
##                                   1998
##
################################################################################
## Live BECOs from 1998
beco98 <- pp %>% filter(STAT98 == "ALIVE", !is.na(DBH98), SPEC == toupper(spec),
                        !is.na(HTTCR98)) %>%
                            select(PPLOT, ELEVCL, ASPCL, ends_with("98"), ELEV)
ps98 <- readRDS(paste0("./", spec, "_98.rds"))

################################################################################
##
##                                   1986
##
################################################################################
## Use heights measured in 87 for 86 dbhs?
beco86 <- pp %>% filter(STAT86 == "ALIVE", !is.na(DBH86), SPEC == toupper(spec),
                        !is.na(HTTCR86)) %>%
                            select(PPLOT, ELEVCL, ASPCL, ends_with("86"), ELEV)
ps86 <- readRDS("./beco_86.rds")

################################################################################
##
##                                   1987
##
################################################################################
## No height measurements
## beco87 <- pp %>% filter(STAT87 == "ALIVE", !is.na(DBH87), toupper(spec),
##                         !is.na(HTTCR87)) %>%
##                             select(PPLOT, ELEVCL, ASPCL, ends_with("87"), ELEV)

################################################################################
##
##                                 Run fits
##
################################################################################
yrs <- c(86, 98, 10)
for (yr in yrs) {
    dat <- get(paste0("beco",yr))
    ps <- get(paste0("ps",yr))
    ps$gamma <- NULL  # decided to kill intercept param

    method <- "SANN" # "Nelder-Mead"
    maxit <- 1e6

    summary(fit <- run_fit(dat, ps, yr, method=method, maxit=maxit))
    summary(fit2 <- run_fit(dat, as.list(coef(fit)), yr))

    ## save parameters
    ps <- as.list(coef(fit2))
    saveRDS(ps, file=paste0("~/work/ecodatascripts/vars/heights/gompertz/elev/beco/beco_", yr, ".rds"))
}
