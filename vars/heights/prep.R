### prep.R --- 
## Filename: prep.R
## Description: Make working datasets with variables for fitting
## Author: Noah Peart
## Created: Fri Mar 13 17:12:40 2015 (-0400)
## Last-Updated: Fri Mar 20 13:32:28 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/read/read-moose.R")
source("~/work/ecodatascripts/read/read-transect.R")
source("~/work/ecodatascripts/vars/heights/canopy/load_canopy.R")
library(dplyr)
library(lazyeval)


prep_data <- function(dat, yr, spec, can_func="can_hts") {
    require(dplyr)
    require(lazyeval)
    ## variable names
    stat <- paste0("STAT", yr)
    dbh <- paste0("DBH", yr)
    ht <- paste0("HTTCR", yr)
    
    ## Conditions
    is_alive <- interp(~stat == "ALIVE", stat=as.name(stat))
    has_dbh <- interp(~!is.na(dbh), dbh=as.name(dbh))
    has_ht <- interp(~!is.na(ht), ht=as.name(ht))
    res <- dat %>% filter_(~PPLOT > 3, is_alive, has_dbh, has_ht, ~SPEC %in% toupper(spec))

    ## Canopy (defined by can_func)
    res$canht <- apply(res, 1, function(x) do.call(can_func, list(row=x, yr=yr)))
    res
}


## high high transect data
prep_hh <- function(dat, yr, spec, can_func="can_hh") {
    require(dplyr)
    require(lazyeval)
    ## variable names
    stat <- paste0("STAT", yr)
    dbh <- paste0("DBH", yr)
    ht <- paste0("HT", yr)
    
    ## Conditions
    is_alive <- interp(~stat == "ALIVE", stat=as.name(stat))
    has_dbh <- interp(~!is.na(dbh), dbh=as.name(dbh))
    has_ht <- interp(~!is.na(ht), ht=as.name(ht))
    res <- dat %>% filter_(~ELEVCL == "HH", is_alive, has_dbh, has_ht,
                           ~SPEC %in% toupper(spec))

    ## Canopy (defined by can_func)
    res$canht <- apply(res, 1, function(x) do.call(can_func, list(row=x, yr=yr)))
    res
}
