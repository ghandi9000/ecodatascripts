### read-transect.R --- 
## Filename: read-transect.R
## Description: Moosilauke transect data
## Author: Noah Peart
## Created: Fri Mar 13 12:45:59 2015 (-0400)
## Last-Updated: Thu Apr  2 23:44:00 2015 (-0400)
##           By: Noah Peart
######################################################################
## Note: Subset by HH to get high high transects
if (Sys.info()['sysname'] == "Linux") {
    tp <- read.csv("~/Dropbox/Shared/Data/TRSAPTRMAS11.csv")
} else if ("noah" %in% list.files("C:\\Users")) {
    tp <- read.csv("C:/Users/noah/Dropbox/Shared/Data/TRSAPTRMAS11.csv")
} else {
    cat("\n\n\t\t*** Couldn't find moosilauke data! ***
\n\t\tSet the location in 'ecodatascripts/read-moose.R'.\n\n")
    
    ##
    ##                          Set path to data here!
    ##

    ## Fill in path and uncomment the following line
    ## tp <- read.csv("path/to/data")

    stop("Not run.")  # remove this
}
