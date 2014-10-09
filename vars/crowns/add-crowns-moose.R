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

## Predict crown dimensions
## NOTE: some of these predictions will get overwritten with non-predicted values
##  in the next piece of code.
yrs <- c(86, 87, 98, 10)  # years to predict for
elevs <- c("L", "M", "H")
specs <- list("abba" = "ABBA",
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
            rows <- dat$spec %in% specs[[specGroup]] & !is.na(dat[, ba]) & !is.na(dat[, ht])
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
dat[depthRows86, "crarea86"] <- dat[depthRows86, "crht86"]
dat[depthRows87, "crarea87"] <- dat[depthRows87, "crht87"]

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

## Deal with negative values
## - Set all estimated crown areas that are negative to be the minimum of observed areas
cols <- grep("^crarea[0-9]|^crdepth[0-9]", names(dat))
for (col in names(dat)[cols]) {
    negs <- which(dat[, col] <= 0)
}

## Write data
write.csv(dat, "~/work/data/moose/moose-wide.csv", row.names=FALSE)

## Cleanup
rm(list=ls())
