### canopy.R --- 
## Filename: canopy.R
## Description: Estimate canopy height/dbh
## Author: Noah Peart
## Created: Mon Mar  2 14:31:12 2015 (-0500)
## Last-Updated: Mon Mar  2 15:00:05 2015 (-0500)
##           By: Noah Peart
######################################################################
library(plyr)
source("~/work/ecodatascripts/read/read-moose.R")
yrs <- c(86, 87, 98, 10) ## yrs to calculate canopy heights

canhts <- ddply(pp, .(PPLOT), .fun = function(x) {
    x <- droplevels(x)
    nCodoms <- sapply(yrs, function(y) {
        yr <- y
        if (unique(x$PPLOT) == 16 && yr == 87 ||
            unique(x$PPLOT) == 22 && yr == 87) { yr <- 98 } ## use trees from 98 for plots 16&22
        stat <- x[, paste0("STAT",yr)]
        if(unique(x$PPLOT) == 15 && yr == 86) yr <- 87 ## use heights from 87
        htCol <- x[, paste0("HTTCR",yr)]
        pp <- x[!is.na(htCol) & stat == "ALIVE",]
        ifelse (y == 86, yy <- 88, yy <- yr) ## use can heights from 88 for 86 plots
        cpos <- paste0("CPOS", yy)
        codHeights <- pp[pp[, cpos] == "c", paste0("HTTCR",yr)]
        mean(codHeights) ## calculate canopy heights
    })
})
names(canhts) <- c("PPLOT", paste0("CANHT",yrs))

## Canopy dbhs
candbhs <- ddply(pp, .(PPLOT), .fun = function(x) {
    x <- droplevels(x)
    canDBH <- sapply(yrs, function(y) {
        yr <- y
        canHt <- canhts[canhts$PPLOT==unique(x$PPLOT),paste0("CANHT",yr)]
        stat <- x[, paste0("STAT",yr)]
        ifelse (unique(x$PPLOT) == 15 & yr == 86, ## use heights from 87 for plot 15
            { htCol <- x[, paste0("HTTCR",87)]; colName <- paste0("HTTCR",87) },
               { htCol <- x[, paste0("HTTCR", yr)]; colName <- paste0("HTTCR",yr) })
        pp <- x[!is.na(htCol) & stat == "ALIVE",]
        dbhs <- pp[which(pp[, colName] > canHt), paste0("DBH", yr)]
        mean(dbhs, na.rm = TRUE) ## calculate canopy heights
    })
})
names(candbhs) <- c("PPLOT", paste0("CANDBH",yrs))

