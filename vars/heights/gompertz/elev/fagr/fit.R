### fit.R --- 
## Filename: fit.R
## Description: FAGR elevation only
## Author: Noah Peart
## Created: Thu Mar 12 19:19:39 2015 (-0400)
## Last-Updated: Thu Mar 12 20:03:26 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/read/read-moose.R")
source("~/work/ecodatascripts/vars/heights/gompertz/elev/model.R")
library(dplyr)
library(magrittr)

spec <- c("fagr")
setwd("~/work/ecodatascripts/vars/heights/gompertz/elev/fagr")  # for pars

################################################################################
##
##                                   2010
##
################################################################################
## Only 5 trees
fagr10 <- pp %>% filter(STAT10 == "ALIVE", !is.na(DBH10), SPEC %in% toupper(spec),
                        !is.na(HTTCR10)) %>%
                            select(PPLOT, ELEVCL, ASPCL, ends_with("10"), ELEV, TAG)
## ps10 <- readRDS(paste0("./fagr_10.rds"))  # fit parameters

################################################################################
##
##                                   1998
##
################################################################################
## 5 trees
fagr98 <- pp %>% filter(STAT98 == "ALIVE", !is.na(DBH98), SPEC %in% toupper(spec),
                        !is.na(HTTCR98)) %>%
                            select(PPLOT, ELEVCL, ASPCL, ends_with("98"), ELEV, TAG)
## ps98 <- readRDS(paste0("./fagr_98.rds"))

################################################################################
##
##                                   1986
##
################################################################################
## No trees
fagr86 <- pp %>% filter(STAT86 == "ALIVE", !is.na(DBH86), SPEC %in% toupper(spec),
                        !is.na(HTTCR86)) %>%
                            select(PPLOT, ELEVCL, ASPCL, ends_with("86"), ELEV, SPEC)
## ps86 <- readRDS("./fagr_86.rds")

################################################################################
##
##                                   1987
##
################################################################################
## No height measurements
fagr87 <- pp %>% filter(STAT87 == "ALIVE", !is.na(DBH87), SPEC %in% toupper(spec),
                        !is.na(HTTCR87)) %>%
                            select(PPLOT, ELEVCL, ASPCL, ends_with("87"), ELEV)

################################################################################
##
##                                 Run fits
##
################################################################################
## yrs <- c(86, 98, 10)
## for (yr in yrs) {
##     dat <- get(paste0("fagr",yr))
##     ps <- get(paste0("ps",yr))
##     ps$gamma <- NULL  # decided to kill intercept param

##     method <- "SANN" # "Nelder-Mead"
##     maxit <- 1e6

##     summary(fit <- run_fit(dat, ps, yr, method=method, maxit=maxit))  # SANN first
##     summary(fit2 <- run_fit(dat, as.list(coef(fit)), yr))             # the Nelder-Mead

##     ## save parameters
##     ps <- as.list(coef(fit))
##     saveRDS(ps, file=paste0("~/work/ecodatascripts/vars/heights/gompertz/elev/fagr/fagr_", yr, ".rds"))
## }

