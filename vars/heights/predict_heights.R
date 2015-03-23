### predict_heights.R --- 
## Filename: predict_heights.R
## Description: Predict missing heights Moosilauke trees (see README.txt for info)
## Author: Noah Peart
## Created: Mon Mar  2 13:37:06 2015 (-0500)
## Last-Updated: Mon Mar 23 16:53:30 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/read/read-moose.R")
source("~/work/ecodatascripts/read/read-transect.R")
source("~/work/ecodatascripts/vars/heights/canopy/load_canopy.R")
library(dplyr)
library(lazyeval)

## Fit directories
basedir <- "~/work/ecodatascripts/vars/heights/"
gomp_full <- paste0(basedir, "gompertz/full/")
gomp_elev <- paste0(basedir, "gompertz/elev/")
gomp_can <- paste0(basedir, "gompertz/elev/")
negexp_full <- paste0(basedir, "negexp/full/")
negexp_elev <- paste0(basedir, "negexp/elev/")

################################################################################
##
##                              Permanent Plots
##
################################################################################
## Use local canopy heights (subplot/plot mean of codominant/dominant tree heights)
## canopy function: can_hts
prep_pp <- function(dat, yr, can_func="can_hts") {
    require(dplyr)
    require(lazyeval)
    ## variable names
    stat <- paste0("STAT", yr)
    dbh <- paste0("DBH", yr)
    ht <- paste0("HTTCR", yr)
    cols <- c("PPLOT", "SPLOT", "ELEV", "SPEC", "TAG")
    
    ## Conditions
    is_alive <- interp(~stat == "ALIVE", stat=as.name(stat))
    has_dbh <- interp(~!is.na(dbh), dbh=as.name(dbh))
    ## has_ht <- interp(~!is.na(ht), ht=as.name(ht))
    res <- dat %>% filter_(~PPLOT > 3, is_alive, has_dbh)

    ## Canopy (defined by can_func)
    res$canht <- apply(res, 1, function(x) do.call(can_func, list(row=x, yr=yr)))
    res[,c(cols, stat, dbh, ht, "canht")]
}

################################################################################
##
##                                   1986
##
################################################################################
## With full Gompertz model (elevation + canopy height)
## Species/groups fit for 1986: ABBA, PIRU
## Use 1998 fits for: maples, hardwoods (maples + prpe + fagr)
## With full negexp model:
## Species: BECO, betula (BECO + rest of betula spp.)
ps_abba <- readRDS(paste0(gomp_full, "abba/abba_86.rds"))
ps_piru <- readRDS(paste0(gomp_full, "piru/piru_86.rds"))
ps_maples <- readRDS(paste0(gomp_full, "maples/maples_98.rds"))
ps_beco <- readRDS(paste0(negexp_full, "beco/beco_86.rds"))
ps_betula <- readRDS(paste0(negexp_full, "betula/betula_86.rds"))
ps_soam <- readRDS(paste0(gomp_full, "soam/soam_10.rds"))
dat <- prep_pp(pp, 86)
sum(is.na(dat$HTTCR86))  # 647 trees to fit


## Get predictions for years/species
## spec: must match individual species or species grouping that was fit
get_preds <- function(spec, years, modtype="gompertz", inds="full", hh=FALSE,
                      cols = list(stat="STAT", ht="HTTCR", dbh="DBH", canht="canht")) {
    species <- unique(pp$SPEC)
    specs <- list(maples=grep("^AC", species, value = T),           # ACSA, ACPE, ACSP
                  hardwoods=grep("^AC|FA", species, value = T),     # maples + FAGR
                  betula=grep("^BE", species, value = T))           # All betulas
    base_dir <- paste0("~/work/ecodatascripts/vars/heights/", modtype, "/", inds, "/")
    source(paste0(base_dir, "model.R"))  # load the model
    par_dir <- paste0(base_dir, "/", tolower(spec), "/")
    keep_cols <- c("SPEC", "ELEV", "canht")
    sppgroup <- spec
    dat <- pp
    if(hh) {
        dat <- tp  # use transect data for HH
        cols$ht = "HT"
    }
    if (spec %in% names(specs)) sppgroup <- specs[[spec]]
    ps <- lapply(years, FUN = function(yr){
        pars <- readRDS(paste0(par_dir, tolower(spec), "_", yr, ".rds"))
        stat <- paste0(cols$stat, yr)
        dbh <- paste0(cols$dbh, yr)
        ht <- paste0(cols$ht, yr)

        ## Prep data
        if (hh) dd <- prep_hh(dat, yr=yr, spec=toupper(sppgroup))
        else dd <- prep_data(dat, yr=yr, spec=toupper(sppgroup))

        ## Get predictions
        if (inds == "full") pred <- {
            do.call(modtype, list(pars, dd[,dbh], dd[,"ELEV"], dd[,"canht"]))
        } else if (inds == "elev") {
            pred <- do.call(modtype, list(pars, dd[,dbh], dd[,"ELEV"]))
        } else {
            pred <- do.call(modtype, list(pars, dd[,dbh], dd[,"canht"]))
        }
        
        res <- cbind(dd[,c(dbh, ht, keep_cols)], pred=pred)
        names(res) <- gsub("[[:digit:]]", "", tolower(names(res)))  # don't track yrs here
        attr(res, "ps") <- pars
        attr(res, "yr") <- yr
        attr(res, "mod") <- get(modtype)
        attr(res, "modname") <- modtype
        attr(res, "inds") <- inds
        attr(res, "spec") <- spec
        attr(res, "hh") <- hh
        attr(res, "cols") <- cols
        res
    })
    names(ps) <- lapply(years, FUN=function(yr) paste0(spec, yr))
    return( ps )
}

