### read-transect.R --- 
## Filename: read-transect.R
## Description: Moosilauke transect data
## Author: Noah Peart
## Created: Fri Mar 13 12:45:59 2015 (-0400)
## Last-Updated: Fri Mar 13 14:07:31 2015 (-0400)
##           By: Noah Peart
######################################################################
## Subset by HH to get high high transects
if (Sys.info()['sysname'] == "Linux") {
    tp <- read.csv("~/Dropbox/Shared/Data/TRSAPTRMAS11.csv")
} else
    tp <- read.csv("C:/Users/noah/Dropbox/Shared/Data/TRSAPTRMAS11.csv")
