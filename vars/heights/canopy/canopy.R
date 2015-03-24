### canopy.R --- 
## Filename: canopy.R
## Description: Estimate canopy height/dbh
## Author: Noah Peart
## Created: Mon Mar  2 14:31:12 2015 (-0500)
## Last-Updated: Mon Mar 23 22:09:46 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/read/read-moose.R")
source("~/work/ecodatascripts/read/read-transect.R")
require(dplyr)

## Returns estimated canopy height and dbh of canopy trees
## (simply the average of trees with certain crown position)
## NOTE: 1. By default uses crown positions from 88 for 86 trees, ("cposyrs" variable)
##       2. For plot 15, heights were measured in 87 (modify "htyrs" for this)
canopy_dims <- function(dat, plot=NULL, yrs=c(86, 87, 98, 10), htyrs = yrs, cposyrs=c(88, 87, 98, 10),
                        cols = list(plot="PPLOT", stat="STAT", ht="HTTCR", dbh="DBH", cpos="CPOS"),
                        canpos = c("c", "d")) {
    if (!missing(plot)) dat <- dat[dat[[cols$plot]] == plot, ]
    if (unique(dat[[cols$plot]]) == 15) htyrs[1] <- 87  # plot 15 hts measured in 87 for 86 dbhs
    res <- sapply(1:length(yrs), FUN = function(i) {
        inds <- (dat[[paste0(cols$stat, yrs[i])]] == "ALIVE") &
            (!is.na(dat[[paste0(cols$ht, htyrs[i])]])) &
                (dat[[paste0(cols$cpos, cposyrs[i])]] %in% canpos)
        c(ht_mean = mean(dat[inds,][[paste0(cols$ht, htyrs[i])]]),    # canopy height mean
          ht_med = median(dat[inds,][[paste0(cols$ht, htyrs[i])]]),   # canopy height median
          ht_dev = sd(dat[inds,][[paste0(cols$ht, htyrs[i])]]),       # canopy height sd
          dbh_mean= mean(dat[inds,][[paste0(cols$dbh, yrs[i])]]),     # canopy dbh mean 
          dbh_med = median(dat[inds,][[paste0(cols$dbh, yrs[i])]]),   # canopy dbh median
          dbh_dev = sd(dat[inds,][[paste0(cols$dbh, yrs[i])]]),       # canopy dbh sd
          cod = sum(dat[inds,][[paste0(cols$cpos, cposyrs[i])]] == "c"),
          dom = sum(dat[inds,][[paste0(cols$cpos, cposyrs[i])]] == "d"),
          count = nrow(dat[inds,]))
    })
    data.frame(t(res), time = yrs)
}


## Canopy height estimated by mean of tallest x (default is 5) trees in plot
## NOTE: user to pass proper number of trees
canopy_tall_x <- function(dat, plot=NULL, num_trees=5, yrs=c(99, 11),
                      cols = list(plot="TPLOT", stat="STAT", ht="HT")) {
    if (!missing(plot)) dat <- dat[dat[[cols$plot]] == plot, ]
    res <- sapply(1:length(yrs), FUN = function(i) {
        inds <- (dat[[paste0(cols$stat, yrs[i])]] == "ALIVE") &
            (!is.na(dat[[paste0(cols$ht, yrs[i])]]))
        tall_x <- sort(dat[inds,][[paste0(cols$ht, yrs[i])]], decreasing = T)[1:num_trees]
        c(ht_mean = mean(tall_x),    # canopy height mean
          ht_med = median(tall_x),   # canopy height median
          ht_dev = sd(tall_x))       # canopy height sd
      })
    data.frame(t(res), time = yrs)
}

## Run on pplot and splot/pplot
can_splot <- pp %>% filter(PPLOT > 3 & !is.na(SPLOT)) %>% group_by(PPLOT, SPLOT) %>%
    do(canopy_dims(., canpos=c("c","d")))
can_plot <- pp %>% filter(PPLOT > 3) %>% group_by(PPLOT) %>%
    do(canopy_dims(., canpos=c("c","d")))

## Run on hh transect plots (tallest 5 trees)
can_hh <- tp %>% filter(ELEVCL == "HH") %>% group_by(TPLOT, TRAN) %>%
    do(canopy_tall_x(., num_trees=5))

## Save data
## write.csv(can_splot, "~/work/ecodatascripts/vars/heights/canopy/can_splot.csv", row.names=F)
## write.csv(can_plot, "~/work/ecodatascripts/vars/heights/canopy/can_plot.csv", row.names=F)
## write.csv(can_hh, "~/work/ecodatascripts/vars/heights/canopy/hh_plot.csv", row.names=F)

## Compare to previous estimates
## prev <- distinct(pp, cht8687, cht98, CHT10) %>% select(PPLOT, cht8687, cht98, CHT10) %>%
##     filter(complete.cases(.)) %>% print
## nw <- reshape(data.frame(can_dims), direction="wide", timevar="time", idvar="PPLOT") %>%
##     mutate(ht8687 = na.omit(c(ht.86,ht.87))) %>% select(PPLOT, ht8687, ht.98, ht.10) %>% print

## Visualize
## with(can_dims, stripchart(time~ht, col = PPLOT, jitter=1))
## p4 <- pp %>% filter(PPLOT == 4, STAT86 == "ALIVE") %>% select(ends_with("86"))
## p4can <- can_dims %>% filter(PPLOT == 4, time == 86)
## with(p4, plot(DBH86, HTTCR86))

## p9 <- pp %>% filter(PPLOT == 9, STAT86 == "ALIVE") %>% select(ends_with("86"))
## p9can <- can_dims %>% filter(PPLOT == 9, time == 86)
## with(p9, plot(DBH86, HTTCR86))
## points(p9can$dbh, p9can$ht, pch = 16, col = "darkred")
