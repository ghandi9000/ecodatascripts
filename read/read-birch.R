### read-birch.R --- 
## Filename: read-birch.R
## Description: Extra BECO data
## Author: Noah Peart
## Created: Tue Mar 17 20:04:53 2015 (-0400)
## Last-Updated: Tue Mar 17 20:06:21 2015 (-0400)
##           By: Noah Peart
######################################################################
if (Sys.info()['sysname'] == "Linux") {
    becos <- read.csv("~/Dropbox/Shared/Data/BIRCH_RML.csv")
} else
    becos <- read.csv("C:/Users/noah/Dropbox/Shared/Data/BIRCH_RML.csv")
