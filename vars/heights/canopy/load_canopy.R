### load_canopy.R --- 
## Filename: load_canopy.R
## Description: Load canopy variables
## Author: Noah Peart
## Created: Fri Mar 13 16:59:40 2015 (-0400)
## Last-Updated: Thu Mar 19 17:24:28 2015 (-0400)
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
    return(ifelse(!is.na(can1), can1, can2))
}

## Local canopy heights
mfunc <- function(val) return(val)  # can be altered for more advanced measurments
can_local <- function(plot, targ, yr, measure="mfunc") {
##     inds <- with(plot, plot[targ$TAG != TAG & ]
}

## use tallest 5 trees for hh transect canopy estimates
can_hh <- function(row, yr, measure="ht_mean") {
    res <- with(hh_plot, hh_plot[time==yr & TPLOT==as.numeric(row[["TPLOT"]]), measure])
    return( res )
}

