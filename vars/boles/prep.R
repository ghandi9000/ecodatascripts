### prep.R --- 
## Filename: prep.R
## Description: Prep data for bole volume estimates
## Author: Noah Peart
## Created: Thu Apr  2 11:08:46 2015 (-0400)
## Last-Updated: Thu Apr  2 11:52:11 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/vars/heights/predict_heights.R")  # rerun to predict heights to pp/tp
source("~/work/ecodatascripts/vars/boles/functions.R")          # BV equations

## 'pp' is permanent plot data
## 'tp' is transect data

################################################################################
##
##                                   ABBA
##
################################################################################
## Kozak eq. for dbh > 1.3, height > 1.3
## Honer eq. for all

## permanent plots
ppyrs <- c(86, 87, 98, 10)
htcol <- "htpred"  # height column to use
dbhcol <- "DBH"
speccol <- "SPEC"
minht <- 1.3  # minimum height
mindbh <- 1.3
spec <- "ABBA"

for (yr in ppyrs) {
    ht <- paste0(htcol, yr)
    dbh <- paste0(dbhcol, yr)
    inds <- !is.na(pp[, ht]) & !is.na(pp[, dbh]) & pp[, dbh] > mindbh &
        pp[, ht] > minht & pp[, speccol] == spec
    
    pp[inds, paste0("kozak", yr)] <- bolevol(dbh=pp[,dbh], ht=pp[,ht], species=spec)
}


