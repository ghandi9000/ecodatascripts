### load_canopy.R --- 
## Filename: load_canopy.R
## Description: Load canopy variables
## Author: Noah Peart
## Created: Fri Mar 13 16:59:40 2015 (-0400)
## Last-Updated: Fri Mar 27 18:33:39 2015 (-0400)
##           By: Noah Peart
######################################################################
## Canopy dimensions (permanent plots)
can_splot <- read.csv("~/work/ecodatascripts/vars/heights/canopy/can_splot.csv")
can_plot <- read.csv("~/work/ecodatascripts/vars/heights/canopy/can_plot.csv")
hh_plot <- read.csv("~/work/ecodatascripts/vars/heights/canopy/hh_plot.csv")

## Function to retrieve canopy height for a subplot/pplot (interactive)
## Uses canopy height measure from subplot if possible, otherwise
## uses canopy height measured across entire plot
can_ht <- function(plot, splot, yr, measure="ht_mean") {
    can1 <- with(can_splot,
                 can_splot[time==yr & PPLOT==plot & SPLOT==splot, measure])
    can2 <- with(can_plot,
                 can_plot[time==yr & PPLOT==plot, measure])
    ifelse(!is.na(can1), can1, can2)
}

## apply to rows to add canopy hts
can_hts <- function(row, yr, measure="ht_mean") {
    can1 <- with(can_splot,
                 can_splot[time==yr & PPLOT==as.numeric(row[["PPLOT"]]) &
                               SPLOT==as.numeric(row[["SPLOT"]]), measure])
    can2 <- with(can_plot,
                 can_plot[time==yr & PPLOT==as.numeric(row[["PPLOT"]]), measure])

    if (!length(can2)) return( NA )
    return(ifelse(!is.na(can1) || !length(can1), can1, can2))
}

## add canopy to permanent plots, vectorized
## use this version for actually adding to dataset (w/ dplyr)
can_pp <- function(dat, plot, splot, yrs = c(86, 87, 98, 10), measure="ht_mean") {
    for (yr in yrs) {
        can1 <- with(can_splot,
                     can_splot[time==yr & PPLOT==plot & SPLOT==splot, measure])
        can2 <- with(can_plot,
                     can_plot[time==yr & PPLOT==plot, measure])
        can <- ifelse(!is.na(can1), can1, can2)
        can <- ifelse(!length(can2), NA, can)
        ##    cat(paste("Plot", plot, ", Splot", splot, ", Canopy height:", can, "\n"))
        dat[, paste0("canht", yr)] <- rep(can, nrow(dat))
    }
    dat
}

## Local canopy heights
## mfunc <- function(val) return(val)  # can be altered for more advanced measurments
## can_local <- function(plot, targ, yr, measure="mfunc") {
## ##     inds <- with(plot, plot[targ$TAG != TAG & ]
## }

## use tallest 5 trees for hh transect canopy estimates
can_hh <- function(row, yr, measure="ht_mean") {
    res <- with(hh_plot, hh_plot[time==yr & TPLOT==as.numeric(row[["TPLOT"]]) &
                                   TRAN==row[["TRAN"]], measure])
    return( res )
}

can_hh_add <- function(dat, tran, tplot, yrs = c(87, 98, 99, 10, 11), measure="ht_mean") {
    for (yr in yrs) {
        res <- with(hh_plot, hh_plot[time==yr & TPLOT==tplot & TRAN==tran, measure])
        res <- ifelse(!length(res), NA, res)
        ##    cat(paste("Plot", plot, ", Splot", splot, ", Canopy height:", can, "\n"))
        dat[, paste0("canht", yr)] <- rep(res, nrow(dat))
    }
    dat
}
