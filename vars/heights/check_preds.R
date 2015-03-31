### check_preds.R --- 
## Filename: check_preds.R
## Description: Check height predictions (visualize)
## Author: Noah Peart
## Created: Mon Mar 30 13:50:55 2015 (-0400)
## Last-Updated: Mon Mar 30 17:17:42 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/vars/heights/predict_heights.R")  # runs predictions

################################################################################
##
##                           Permanent plots
##
################################################################################
## Heights not fit: check species
missed <- pp[pp$PPLOT > 3 & !is.na(pp$DBH86) & is.na(pp$htpred86), ]
table(missed$SPEC) # should be just UNID and/or ""

## Look at HT vs. DBH for species
specs <- names(params_pp)
yr <- 86

stat <- paste0("STAT", yr)
dbh <- paste0("DBH", yr)
ht <- paste0("HTTCR", yr)
pred <- paste0("htpred", yr)

par(mfrow=c(4, 3))
for (spp in specs) {
    inds <- pp$SPEC == toupper(spp) & pp[[stat]] == "ALIVE" & pp$PPLOT > 3
    if (sum(inds) == 0) cat(paste("No data for", spp, "\n"))
    else {
        ylims = c(min(c(pp[inds, ht], pp[inds, pred]), na.rm=TRUE) - 1,
        max(c(pp[inds, ht], pp[inds, pred]), na.rm = TRUE) + 1)
        plot(pp[inds, dbh], pp[inds, ht], ylim=ylims, main = paste("Species:", spp, ", Year:", yr))
        points(pp[inds, dbh], pp[inds, pred], col="red", pch=16)
    }
}

################################################################################
##
##                                 Transects
##
################################################################################
## Elevation only fit (use for all except HH elevation)
## Heights not fit: check species
yrs <- c(87, 98, 99, 10, 11)
for (yr in yrs) {
    htcol <- paste0("htpred", yr)
    dbhcol <- paste0("DBH", yr)
    missed <- tp[!is.na(tp[[dbhcol]]) & is.na(tp[[htcol]]), ]
    cat(paste("Year", yr, "\n"))
    print(table(missed$SPEC)) # should be just UNID and/or ""
    cat("\n")
}

## Look at HT vs. DBH for species
specs <- names(params_tp_low)
yr <- 11

stat <- paste0("STAT", yr)
dbh <- paste0("DBH", yr)
ht <- paste0("HT", yr)
pred <- paste0("pred", yr)

par(mfrow=c(4, 4))
for (spp in specs) {
    inds <- tp$SPEC == toupper(spp) & tp[[stat]] == "ALIVE"
    if (sum(inds) == 0) cat(paste("No data for", spp, "\n"))
    else {
        ylims = c(min(c(tp[inds, ht], tp[inds, pred]), na.rm=TRUE) - 1,
        max(c(tp[inds, ht], tp[inds, pred]), na.rm = TRUE) + 1)
        plot(tp[inds, dbh], tp[inds, ht], ylim=ylims, main = paste("Species:", spp, ", Year:", yr))
        points(tp[inds, dbh], tp[inds, pred], col="red", pch=16)
    }
}

################################################################################
##
##                       HH heights (canopy only fits)
##
################################################################################
## Heights not fit: check species
yrs <- c(87, 98, 99, 10, 11)
for (yr in yrs) {
    htcol <- paste0("HHhtpred", yr)
    dbhcol <- paste0("DBH", yr)
    missed <- tp[!is.na(tp[[dbhcol]]) & is.na(tp[[htcol]]) & tp$ELEVCL=="HH" &
                    !(tp$TRAN == "E335" & tp$TPLOT == 16) & !grepl("^S", tp$TRAN), ]
    cat(paste("Year", yr, "\n"))
    print(table(missed$SPEC)) # should be just UNID and/or ""
    cat("\n")
}

## Look at HT vs. DBH for species
specs <- names(params_tp_hh)
yr <- 99

stat <- paste0("STAT", yr)
dbh <- paste0("DBH", yr)
ht <- paste0("HT", yr)
pred <- paste0("pred", yr)

par(mfrow=c(2, 2))
for (spp in specs) {
    inds <- tp$SPEC == toupper(spp) & tp[[stat]] == "ALIVE" & tp$ELEVCL == "HH"
    if (sum(inds) == 0 || spp == "PIRU") cat(paste("No data for", spp, "\n"))
    else {
        ylims = c(min(c(tp[inds, ht], tp[inds, pred]), na.rm=TRUE) - 1,
        max(c(tp[inds, ht], tp[inds, pred]), na.rm = TRUE) + 1)
        plot(tp[inds, dbh], tp[inds, ht], ylim=ylims, main = paste("Species:", spp, ", Year:", yr))
        points(tp[inds, dbh], tp[inds, pred], col="red", pch=16)
    }
}
