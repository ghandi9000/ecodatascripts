################################################################################
##
##                Add Crown Variables to Moosilauke Data
##
## When crown dimensions are unavailable, crown variables are predicted
##  using models created in "../models/fit-models.R"
##
## This script creates 'crdepth'/'ecrdepth' and 'crarea'/'ecrarea' variables for each year
## The 'e' prepended means 'estimated', so these columns are boolean
##
################################################################################
source("~/work/ecodatascripts/vars/crowns/model/fit-models.R")
dat <- read.csv("~/work/data/moose/moose-wide.csv")

################################################################################
##
##                         Predict crown dimensions
##
################################################################################
## NOTE: some of these predictions will get overwritten with non-predicted values
##  in the next piece of code.
yrs <- c(86, 87, 98, 10)                                  # years to predict for
elevs <- c("L", "M", "H")                                 # elevations
specs <- list("abba" = "ABBA",                            # species groupings
              "piru" = "PIRU",
              "beco" = c("BECO", "BEAL", "BEPA"))
specs[["hard"]] <- levels(dat$spec)[!(levels(dat$spec) %in%
                                      c(unlist(specs)))]  # fit all these with hardwood model

for (yr in yrs) {
    crarea <- paste0("crarea", yr)
    crdepth <- paste0("crdepth", yr)
    dat[, crarea] <- rep(NA, nrow(dat))
    dat[, crdepth] <- rep(NA, nrow(dat))
    ba <- paste0("ba", yr)
    ht <- paste0("ht", yr)
    for (elev in elevs) {
        for (specGroup in names(specs)) {
            ## Crown area
            rows <- dat$spec %in% specs[[specGroup]] & !is.na(dat[, ba]) &
                !is.na(dat[, ht]) & dat$elevcl == elev
            pars <- coef(crwnAreaMods[[elev]][[specGroup]])
            dat[rows, crarea] <- pars[["a"]]*dat[rows, ba]^pars[["b"]] - pars[["c"]]*dat[rows, ht]
            ## Crown depth
            pars <- coef(crwnDepthMods[[elev]][[specGroup]])
            dat[rows, crdepth] <- pars[["(Intercept)"]] + pars[["ht"]]*dat[rows, ht]
        }
    }
}

## Compute crown variables from available data:
## Crown area (horizontal cut through crown at widest point): pi * a * b
## 'a' and 'b' are long and short axes of ellipse (cperp and clong)
## Crown depth:
## Labeled as crht86 and crht87
areaRows86 <- !is.na(dat$cperp86) & !is.na(dat$clong86)
depthRows86 <- !is.na(dat$crht86)
areaRows87 <- !is.na(dat$cperp87) & !is.na(dat$clong87)
depthRows87 <- !is.na(dat$crht87)

dat[areaRows86, "crarea86"] <- pi * (dat[areaRows86, "cperp86"]/2) *
    (dat[areaRows86, "clong86"]/2)
dat[areaRows87, "crarea87"] <- pi * (dat[areaRows87, "cperp87"]/2) *
    (dat[areaRows87, "clong87"]/2)
dat[depthRows86, "crdepth86"] <- dat[depthRows86, "crht86"]
dat[depthRows87, "crdepth87"] <- dat[depthRows87, "crht87"]

## Add boolean estimated columns
for (yr in yrs) {
    areaCol <- paste0("crarea", yr)
    depthCol <- paste0("crdepth", yr)
    eAreaCol <- paste0("ecrarea", yr)
    eDepthCol <- paste0("ecrdepth", yr)
    dat[, eDepthCol] <- rep(NA, nrow(dat))
    dat[, eAreaCol] <- rep(NA, nrow(dat))
    dat[!is.na(dat[, depthCol]), eDepthCol] <- 1
    dat[!is.na(dat[, areaCol]), eAreaCol] <- 1
}
dat[areaRows86, "ecrarea86"] <- 0
dat[areaRows87, "ecrarea87"] <- 0
dat[depthRows86, "ecrdepth86"] <- 0
dat[depthRows87, "ecrdepth87"] <- 0

################################################################################
##
##                    Deal with Negative Crown Dimensions
##
################################################################################
## - Set all estimated crown areas/depths that are negative to be the minimum of
##   observed crown areas for each combination of species group/elevation
## NOTE: all observed areas/depths are from 86/87
areaMins <- dlply(dat, .(elevcl), function(x) {
    mins86 <- lapply(specs, function(sppGroup)
        min(x[x$spec %in% sppGroup & x$ecrarea86 == 0, "crarea86"], na.rm=T))
    mins87 <- lapply(specs, function(sppGroup)
        min(x[x$spec %in% sppGroup & x$ecrarea87 == 0, "crarea87"], na.rm=T))
    pmin(unlist(mins86), unlist(mins87), na.rm=T)
})

depthMins <- dlply(dat, .(elevcl), function(x) {
    mins86 <- lapply(specs, function(sppGroup)
        min(x[x$spec %in% sppGroup & x$ecrdepth86 == 0, "crdepth86"], na.rm=T))
    mins87 <- lapply(specs, function(sppGroup)
        min(x[x$spec %in% sppGroup & x$ecrdepth87 == 0, "crdepth87"], na.rm=T))
    pmin(unlist(mins86), unlist(mins87), na.rm=T)
})

for (yr in yrs) {
    areaCol <- paste0("crarea", yr)
    depthCol <- paste0("crdepth", yr)
    dat[, areaCol] <- apply(dat, 1, function(x) {
        sppGroup <- names(unlist(specs))[match(x[["spec"]], unlist(specs))]
        sppGroup <- gsub("[0-9]", "", sppGroup)
        aMin <- areaMins[[x[["elevcl"]]]][[sppGroup]]
        ifelse(as.numeric(x[[areaCol]]) < aMin, aMin, as.numeric(x[[areaCol]]))
    })
    dat[, depthCol] <- apply(dat, 1, function(x) {
        sppGroup <- names(unlist(specs))[match(x[["spec"]], unlist(specs))]
        sppGroup <- gsub("[0-9]", "", sppGroup)
        dMin <- depthMins[[x[["elevcl"]]]][[sppGroup]]
        ifelse(as.numeric(x[[depthCol]]) < dMin, dMin, as.numeric(x[[depthCol]]))
    })
}

## Write data
write.csv(dat, "~/work/data/moose/moose-wide.csv", row.names=FALSE)

## Cleanup
rm(list=ls())
