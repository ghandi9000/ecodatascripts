### canopy.R --- 
## Filename: canopy.R
## Description: Estimate canopy height/dbh
## Author: Noah Peart
## Created: Mon Mar  2 14:31:12 2015 (-0500)
## Last-Updated: Wed Mar  4 20:08:55 2015 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/read/read-moose.R")
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
        c(ht = mean(dat[inds,][[paste0(cols$ht, htyrs[i])]]),   # canopy height
          dbh = mean(dat[inds,][[paste0(cols$dbh, yrs[i])]]))   # canopy dbh
    })
    data.frame(t(res), time = yrs)
}


## All plots canopy heights
can_dims <- pp %>% group_by(PPLOT) %>% do(canopy_dims(., canpos=c("c","d"))) %>% filter(PPLOT > 3)

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
