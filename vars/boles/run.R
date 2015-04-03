#!/usr/bin/env Rscript
### run.R --- 
## Filename: run.R
## Description: Wrapper to run height/bole calculations
## Author: Noah Peart
## Created: Thu Apr  2 22:43:07 2015 (-0400)
## Last-Updated: Fri Apr  3 11:36:49 2015 (-0400)
##           By: Noah Peart
######################################################################
cat("\n\n\t\tChecking/installing packages...\n\n")
packages <- c("dplyr", "magrittr", "lazyeval", "bbmle")  # required packages
for (pack in packages)
    if (pack %in% rownames(installed.packages()) == FALSE)
        install.packages(pack)

if (Sys.info()['sysname'] != "Linux") {
    if (!("home" %in% list.files("C:\\")) | !("work" %in% list.files("C:\\home"))) {
        cat("Need to create 'C:\\home\\work' directory and move all these files there.")
    } else { cat("Found C:\\home\\work") }
    Sys.setenv(HOME="C:\\home")  # needed for sourcing
}

cat("\nRunning height and bole volume calculations...\n")
source("./boles.R")
