### predict_heights.R --- 
## Filename: predict_heights.R
## Description: Predict missing heights Moosilauke trees (see README.txt for info)
## Author: Noah Peart
## Created: Mon Mar  2 13:37:06 2015 (-0500)
## Last-Updated: Tue Mar 24 17:25:50 2015 (-0400)
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
    res <- dat %>% filter_(~PPLOT > 3, is_alive, has_dbh)

    ## Canopy (defined by can_func)
    res$canht <- apply(res, 1, function(x) do.call(can_func, list(row=x, yr=yr)))
    res[,c(cols, stat, dbh, ht, "canht")]
}

pars <- list(
    ABBA = list(spp = "abba", model = "gompertz", inds = "full", yrs = c(86, 98, 98, 10)),
    PIRU = list(spp = "piru", model = "gompertz", inds = "full", yrs = c(86, 87, 98, 10)),

    ## Betula spp.
    BECO = list(spp = "beco", model = "negexp", inds = "full", yrs = c(86, 98, 98, 10)),
    BEPA = list(spp = "betula", model = "negexp", inds = "full", yrs = c(86, 98, 98, 10)),
    BEAL = list(spp = "betula", model = "negexp", inds = "full", yr = c(86, 98, 98, 10)),

    ## Maples
    ACSA = list(spp = "maples", model = "gompertz", inds = "full", yrs = c(98, 98, 98, 10)),
    ACPE = list(spp = "maples", model = "gompertz", inds = "full", yrs = c(98, 98, 98, 10)),
    ACSP = list(spp = "maples", model = "gompertz", inds = "full", yrs = c(98, 98, 98, 10)),

    ## Others
    FAGR = list(spp = "hardwoods", model = "gompertz", inds = "full", yrs = c(98, 98, 98, 10)),
    SOAM = list(spp = "soam", model = "gompertz", inds = "full", yrs = c(98, 98, 98, 98)),
    PRPE = list(spp = "soam", model = "gompertz", inds = "full", yrs = c(98, 98, 98, 98)),
    PRVI = list(spp = "soam", model = "gompertz", inds = "full", yrs = c(98, 98, 98, 98))

    ## UNID and unlabeled?
)

fit_pp <- function(dat, pars=pars, basedir=basedir, yrs=c(86, 87, 98, 10), can_func="can_hts") {
    spec <- toupper(spec)
    dat[, paste0("htpred", yrs)] <- NA
    dat[, paste0("canht", yrs)] <- NA
    for (yr in yrs) dat[, paste0("canht", yr)] <- apply(dat, 1, function(x)
        do.call(can_func, list(row=x, yr=yr)))  # add canopy heights

    for (spp in levels(dat$SPEC)) {
        if (spp %in% names(pars)) {  # ignore unidentified species
            sprintf(spp)
            moddir <- paste0(basedir, pars[[spec]]$model, "/", pars[[spec]]$inds, "/")
            source(paste0(moddir, "model.R"))                        # source the approriate model
            for (i in 1:length(yrs)) {
                ps <- readRDS(paste0(moddir, tolower(pars[[spec]]$spp), "/", tolower(pars[[spec]]$spp),
                                     "_", pars[[spec]]$yrs[i], ".rds"))  # read fit parameters
                inds <- dat$SPEC == spp
            ##     dat[inds, paste0("htpred", yrs[i])] <-
            ##         do.call(pars[[spec]]$model,
            ##                 list(ps = ps,
            ##                      dbh = dat[inds, paste0("DBH", yrs[i])],
            ##                      elev = dat[inds, "ELEV"],
            ##                      canht = dat[inds, paste0("canht", yrs[i])]))
            ## }
        }
    }
    dat
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

