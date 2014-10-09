################################################################################
##
##                  Estimating crown dimensions and shapes
##
## - Ellipsoids for deciduous trees
## - Cones for conifers
##
## NOTE: all data from East aspect
##
################################################################################
## Master data has crown dimensions from 86/87
if (Sys.info()['sysname'] == "Linux") {
    pp <- read.csv("~/Dropbox/Shared/Data/pptreemas10bv.csv")
} else
    pp <- read.csv("C:/Users/noah/Dropbox/Shared/Data/pptreemas10bv.csv")

## Combine data from 86/87 for modeling.
## Note: some trees have measurements in both years.
## Columns to create/combine:
##  crown area, crown depth, basal area, and height
##
## Just using data from 86/87 where crown dimensions were measured
names(pp) <- tolower(names(pp))
dat <- pp[((!is.na(pp$cperp86) & !is.na(pp$clong86))
          | (!is.na(pp$cperp87) & !is.na(pp$clong87))), ]
nrow(dat[!is.na(dat$cperp86) & !is.na(dat$cperp87), ]) # 109 measurements in both yrs
nrow(dat[!is.na(dat$clong86) & !is.na(dat$clong87), ])
sum(is.na(dat$cperp86) & !is.na(dat$cperp87))          # 114 crown dimensions measured in only 1987
sum(!is.na(dat$cperp86) & !is.na(dat$cperp87) &
    !is.na(dat$httcr86) & !is.na(dat$httcr87))

## Remove unused columns
cols <- grep("ba8.|^httcr8.|^dbh8.|crht8.|cperp8.|clong8.|spec|stat8[67]|elevcl|aspcl", names(dat))
dat <- dat[, cols]

## Combine 86/87 data, don't use repeated measurements, favor those from 86
##  since trees dont all have dbh measures in 87
repMeasures <- which( (!is.na(dat$cperp86) & !is.na(dat$clong86)) &
                         (!is.na(dat$cperp87) & !is.na(dat$clong87)) )
dat$dbh <- dat$dbh86              # only have dbh data from 86
dat$ba <- dat$dbh^2 * 0.00007854  # create basal area from dbh
dat$ht <- ifelse(is.na(dat$httcr86), dat$httcr87, dat$httcr86)
dat$cperp <- ifelse(is.na(dat$cperp86), dat$cperp87, dat$cperp86)
dat$clong <- ifelse(is.na(dat$clong86), dat$clong87, dat$clong86)

## Crown area (horizontal cut through crown at widest point): pi * a * b
## 'a' and 'b' are long and short axes of ellipse (cperp and clong)
dat$crarea <- pi * (dat$cperp/2) * (dat$clong/2)

## Crown depth:
## Labeled as crht86 and crht87
dat$crdepth <- ifelse(is.na(dat$crht86), dat$crht87, dat$crht86)  # 25 missing values


################################################################################
##
##                                  Models
##
## Separate models for at low/mid/high elevation and for the following species:
##  ABBA, PIRU, BECO, BEAL
## Also, a generic 'hardwood' model to fit the remaining hardwood species with
##  with small sample sizes
##
## - Crown area (horizontal cut at widest point): a * BA^b - c*HT
## - Crown depth: a * HT
##
################################################################################
crwn_area <- crarea ~ a*ba^b + c*ht    # Crown area model
crwn_depth <- crdepth ~ ht             # Crown depth model

## Fit models
library(plyr)
hardwoods <- c("BEAL", "BECO", "BEPA", "ACSA", "ACPE", "SOAM")

## Crown area models (nonlinear)
crwnAreaMods <- dlply(dat, .(elevcl), function(x) {
    ps <- expand.grid(a=200,   # starting parameters
                      b=seq(0.1,10,length.out=10),
                      c=seq(-1,1,length.out=10))
                      ## d=seq(0.1,10,length.out=10))
    mods <- list("piru"=NULL, "abba"=NULL, "beco"=NULL, "hard"=NULL)
    specGroups <- list("piru" = c("PIRU"),        # species to use for each model
                       "abba" = c("ABBA"),
                       "beco" = c("BECO", "BEAL", "BEPA"),
                       "hard" = hardwoods)
    for (parSet in 1:nrow(ps)) {
        ll <- as.list(ps[parSet, ])
        for (mod in names(mods)) {
            if (is.null(mods[[mod]])) {
                cmodel <- NULL
                try({
                    cmodel <- nls(crwn_area, data = x[x$spec %in% specGroups[[mod]], ],
                                        start = ll)
                }, silent = TRUE)
                if (!is.null(cmodel))
                    mods[[mod]] <- cmodel
            }
        }
    }
    mods
})

## Crown area models (Linear)
crwnAreaLin <- dlply(dat, .(elevcl), function(x) {
    mods <- list("piru"=NULL, "abba"=NULL, "beco"=NULL, "hard"=NULL)
    specGroups <- list("piru" = c("PIRU"),  # species to use for each model
                       "abba" = c("ABBA"),
                       "beco" = c("BECO", "BEAL", "BEPA"),
                       "hard" = hardwoods)
    for (mod in names(mods)) {
        cmodel <- lm(crarea ~ ba + ht, data = x[x$spec %in% specGroups[[mod]], ])
        mods[[mod]] <- cmodel
    }
    mods
})

## Crown depth models
crwnDepthMods <- dlply(dat, .(elevcl), function(x) {
    piruMod <- lm(crwn_depth, data = x[x$spec == "PIRU", ])
    abbaMod <- lm(crwn_depth, data = x[x$spec == "ABBA", ])
    becoMod <- lm(crwn_depth, data = x[x$spec == "BECO" | x$spec == "BEAL" | x$spec == "BEPA", ])
    hardMod <- lm(crwn_depth, data = x[x$spec %in% hardwoods, ])
    list("piru" = piruMod, "abba" = abbaMod, "beco" = becoMod, "hard" = hardMod)
})
