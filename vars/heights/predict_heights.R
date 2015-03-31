### predict_heights.R --- 
## Filename: predict_heights.R
## Description: Predict missing heights Moosilauke trees (see README.txt for info)
## Author: Noah Peart
## Created: Mon Mar  2 13:37:06 2015 (-0500)
## Last-Updated: Mon Mar 30 17:24:38 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/read/read-moose.R")                  # permanent plot data
source("~/work/ecodatascripts/read/read-transect.R")               # transect data
source("~/work/ecodatascripts/vars/heights/pred_functions.R")      # prediction functions
library(dplyr)
library(lazyeval)

## Base directory for height predictions
basedir <- "~/work/ecodatascripts/vars/heights/"

################################################################################
##
##                                Parameters
##
################################################################################
## Permanent plots
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

## Transects: elevation only model
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

## Problems with BECOs at LL
params_tp_low[["BECO"]]$yrs <- rep(10, 5)  # fit all with 2010 fits

## Transects: HH (canopy only model)
params_tp_hh <- list(
    ABBA = list(spp = "abba", model = "gompertz", inds = "can", yrs = c(99, 99, 99, 11, 11)),
    PIRU = list(spp = "abba", model = "gompertz", inds = "can", yrs = c(99, 99, 99, 11, 11)),
    BECO = list(spp = "beco", model = "gompertz", inds = "can", yrs = c(99, 99, 99, 11, 11))
)

################################################################################
##
##                                    Run
##
################################################################################
pp <- fit_pp(pp, params_pp, basedir)
tp <- fit_tp_low(tp, params_tp_low, basedir)
tp <- fit_tp_hh(tp, params_tp_hh, basedir)

## Combine predictions for tp into single "pred" column for each year
yrs <- unique(gsub(".*(..)$", "\\1", grep("^DBH[0-9]+", names(tp), value=T)))
for (yr in yrs) {
    pred <- paste0("pred", yr)
    lowpred <- paste0("htpred", yr)
    hhpred <- paste0("HHhtpred", yr)
    tp[,pred] <- ifelse(tp$ELEVCL == "HH", tp[,hhpred], tp[,lowpred])
}

################################################################################
##
##                                 Problems
##
################################################################################
## - What to do with HH plots that are missing canopy?
## - BECOs not fitting well for LL for 99 data (negexp model with only elevation)
## getting negative heights
yr <- 99
dbh <- paste0("DBH",yr)
pred <- paste0("pred",yr)

skips <- (tp$TRAN == "E335" & tp$TPLOT == 16) | (tp$ELEVCL == "HH" & grepl("^S", tp$TRAN))
missed <- tp[!skips & !is.na(tp[,dbh]) & is.na(tp[,pred]), ]
table(missed$TRAN)
table(missed$TPLOT)
table(missed$SPEC)

## Negative heights -- solved by fitting with 2010 fits
range(tp[,pred], na.rm = T)
sum(!is.na(tp[,pred]) & tp[,pred] < 0)  # 11 BECOs negative in LL plot -4
negs <- tp[!is.na(tp[,pred]) & tp[,pred] < 0, ]
table(negs$SPEC)
table(negs$TPLOT)
