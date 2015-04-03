### boles.R --- 
## Filename: boles.R
## Description: Estimate bole volumes
## Author: Noah Peart
## Created: Thu Apr  2 11:08:46 2015 (-0400)
## Last-Updated: Fri Apr  3 11:45:45 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/vars/heights/predict_heights.R")  # rerun to predict heights to pp/tp
source("~/work/ecodatascripts/vars/boles/functions.R")          # BV equations

## Notes:
## - BV equations are, for the most part, not vectorized.  The kozak eq, "bolevol",
## builds solution with inner loop in current implementation.  The other equations, ie.
## honer and clark, could easily be vectorized by modifying species variables.
## - 'pp' is permanent plot data
## - 'tp' is transect data
cat("\n\n\n\t\t--- Estimating bole volumes ---\n\n\n")

## Permanent plot variables to use (column names, years)
ppyrs <- c(86, 87, 98, 10)
htcol <- "ht"      # height column
dbhcol <- "DBH"    # DBH
speccol <- "SPEC"  # species

## Transect variables
tpyrs <- c(87, 98, 99, 10, 11)
tpdbh <- "DBH"     # DBH column
tpht <- "ht"       # height column
tpspec <- "SPEC"   # species column

## Equation parameters (limits for height and DBH for kozak and clark equations)
lims <- list(ABBA = c(1.3, 1.3), PIRU = c(1.3, 1.5))  # (min DBH, min Height) for kozak
clim <- 5.28                                          # min. height for clark eq.

################################################################################
##
##                              ABBAs and PIRUs
##
################################################################################
## Kozak eq. and first honer eq.

## permanent plots
cat("ABBA and PIRU for permanent plots\n")
specs <- toupper(c("ABBA", "PIRU"))
for (yr in ppyrs) {
    kozak <- paste0("kozak", yr)
    hon <- paste0("honer", yr)
    pp[, kozak] <- NA
    pp[, hon] <- NA
    ht <- paste0(htcol, yr)
    dbh <- paste0(dbhcol, yr)

    for (spec in specs) {
        inds <- !is.na(pp[, ht]) & !is.na(pp[, dbh]) & pp[, dbh] > lims[[spec]][1] &
            pp[, ht] > lims[[spec]][2] & pp[, speccol] == spec
        pp[inds, kozak] <- apply(pp[inds,], 1, FUN = function(r) {
            bolevol(dbh=as.numeric(r[[dbh]]), ht=as.numeric(r[[ht]]), species=r[[speccol]])
        })

        inds <- !is.na(pp[, ht]) & !is.na(pp[, dbh]) & pp[, speccol] == spec
        pp[inds, hon] <- apply(pp[inds,], 1, FUN = function(r) {
            honer(D=as.numeric(r[[dbh]]), H=as.numeric(r[[ht]]), species=r[[speccol]])
        })
    }
}

## Transects
cat("ABBA and PIRU for transect plots\n")
for (yr in tpyrs) {
    kozak <- paste0("kozak", yr)
    hon <- paste0("honer", yr)
    tp[, kozak] <- NA
    tp[, hon] <- NA
    ht <- paste0(tpht, yr)
    dbh <- paste0(tpdbh, yr)

    for (spec in specs) {
        inds <- !is.na(tp[, ht]) & !is.na(tp[, dbh]) & tp[, dbh] > lims[[spec]][1] &
            tp[, ht] > lims[[spec]][2] & tp[, tpspec] == spec
        tp[inds, kozak] <- apply(tp[inds,], 1, FUN = function(r) {
            bolevol(dbh=as.numeric(r[[dbh]]), ht=as.numeric(r[[ht]]), species=r[[tpspec]])
        })

        inds <- !is.na(tp[, ht]) & !is.na(tp[, dbh]) & tp[, tpspec] == spec
        tp[inds, hon] <- apply(tp[inds,], 1, FUN = function(r) {
            honer(D=as.numeric(r[[dbh]]), H=as.numeric(r[[ht]]), species=r[[tpspec]])
        })
    }
}

################################################################################
##
##                             All other species
##
################################################################################
## The rest of the species use clarks eq. and second honer eq. (honer2)
specs <- levels(pp[,speccol])[!(levels(pp[,speccol]) %in% c("ABBA", "PIRU"))]

cat("Other spp. for permanent plots\n")
for (yr in ppyrs) {
    cclark <- paste0("clark", yr)
    choner <- paste0("honer", yr)
    pp[, cclark] <- NA
    ht <- paste0(htcol, yr)
    dbh <- paste0(dbhcol, yr)

    for (spec in specs) {
        inds <- !is.na(pp[, ht]) & !is.na(pp[, dbh]) &  pp[, ht] > clim &
            pp[, speccol] == spec
        if (sum(inds)) {
            pp[inds, cclark] <- apply(pp[inds,], 1, FUN = function(r) {
                clark(dbh=as.numeric(r[[dbh]]), height=as.numeric(r[[ht]]), species=r[[speccol]])
            })
        }
        
        inds <- !is.na(pp[, ht]) & !is.na(pp[, dbh]) & pp[, speccol] == spec
        if (sum(inds)) {
            pp[inds, choner] <- apply(pp[inds,], 1, FUN = function(r) {
                honer2(dbh=as.numeric(r[[dbh]]), height=as.numeric(r[[ht]]), species=r[[speccol]])
            })
        }
    }
}

## Transects
specs <- levels(tp[,speccol])[!(levels(tp[,speccol]) %in% c("ABBA", "PIRU"))]

cat("Other spp. for transect plots\n")
for (yr in tpyrs) {
    cclark <- paste0("clark", yr)
    choner <- paste0("honer", yr)
    tp[, cclark] <- NA
    ht <- paste0(htcol, yr)
    dbh <- paste0(dbhcol, yr)

    for (spec in specs) {
        inds <- !is.na(tp[, ht]) & !is.na(tp[, dbh]) &  tp[, ht] > clim &
            tp[, tpspec] == spec
        if (sum(inds)) {
            tp[inds, cclark] <- apply(tp[inds,], 1, FUN = function(r) {
                clark(dbh=as.numeric(r[[dbh]]), height=as.numeric(r[[ht]]), species=r[[tpspec]])
            })
        }
        
        inds <- !is.na(tp[, ht]) & !is.na(tp[, dbh]) & tp[, tpspec] == spec
        if (sum(inds)) {
            tp[inds, choner] <- apply(tp[inds,], 1, FUN = function(r) {
                honer2(dbh=as.numeric(r[[dbh]]), height=as.numeric(r[[ht]]), species=r[[tpspec]])
            })
        }
    }
}

################################################################################
##
##                            Combine BV columns
##
################################################################################
## When available, Kozak and Clark boles are preferred over Honer
## Init columns
pp[, paste0("bv", ppyrs)] <- NA
tp[, paste0("bv", tpyrs)] <- NA

## abba/piru
inds <- pp[,speccol] %in% c("ABBA", "PIRU")
for (yr in ppyrs) {
    ckozak <- paste0("kozak", yr)
    choner <- paste0("honer", yr)
    bv <- paste0("bv", yr)
    pp[inds, bv] <- pp[inds, ckozak]                                    # give preference to kozak estimates
    pp[inds & is.na(pp[,bv]), bv] <- pp[inds & is.na(pp[,bv]), choner]  # honer fills holes
}

inds <- tp[,tpspec] %in% c("ABBA", "PIRU")
for (yr in tpyrs) {
    ckozak <- paste0("kozak", yr)
    choner <- paste0("honer", yr)
    bv <- paste0("bv", yr)
    tp[inds, bv] <- tp[inds, ckozak]
    tp[inds & is.na(tp[,bv]), bv] <- tp[inds & is.na(tp[,bv]), choner]
}

## Rest of spp.
inds <- !(pp[,speccol] %in% c("ABBA", "PIRU"))
for (yr in ppyrs) {
    cclark <- paste0("clark", yr)
    choner <- paste0("honer", yr)
    bv <- paste0("bv", yr)
    pp[inds, bv] <- pp[inds, cclark]                                    # give preference to clark estimates
    pp[inds & is.na(pp[,bv]), bv] <- pp[inds & is.na(pp[,bv]), choner]  # honer fills holes
}

inds <- !(tp[,tpspec] %in% c("ABBA", "PIRU"))
for (yr in tpyrs) {
    cclark <- paste0("clark", yr)
    choner <- paste0("honer", yr)
    bv <- paste0("bv", yr)
    tp[inds, bv] <- tp[inds, cclark]
    tp[inds & is.na(tp[,bv]), bv] <- tp[inds & is.na(tp[,bv]), choner]
}

################################################################################
##
##                                   Check
##
################################################################################
check <- FALSE
if (check) {
    ## Permanent plots
    yr <- 10
    pred <- "kozak"

    pcol <- paste0(pred, yr)
    ht <- paste0(htcol, yr)
    dbh <- paste0(dbhcol, yr)
    specs <- c("ABBA", "PIRU")
    if (pred == "clark") specs <- levels(pp[,speccol])[!(levels(pp[,speccol]) %in% c("ABBA","PIRU"))]
    if (pred == "kozak") specs <- c("ABBA", "PIRU")
    missing <- !is.na(pp[,ht]) & !is.na(pp[,dbh]) & is.na(pp[,pcol]) & pp[,speccol] %in% specs

    m <- pp[missing,]
    cat(paste("Missing:", sum(missing), "\n"))
    cat("Species\n")
    table(m[,speccol])
    cat("DBH range\n")
    print(summary(m[,dbh]))
    cat("Height\n")
    print(summary(m[,ht]))

    plot(m[,dbh], m[,ht])

    ## Transects
    pcol <- paste0(pred, yr)
    ht <- paste0(tpht, yr)
    dbh <- paste0(tpdbh, yr)
    specs <- c("ABBA", "PIRU")
    if (pred == "clark") specs <- levels(tp[,speccol])[!(levels(tp[,speccol]) %in% c("ABBA","PIRU"))]
    if (pred == "kozak") specs <- c("ABBA", "PIRU")
    missing <- !is.na(tp[,ht]) & !is.na(tp[,dbh]) & is.na(tp[,pcol]) & tp[,speccol] %in% specs

    tm <- tp[missing,]
    cat(paste("Missing:", sum(missing), "\n"))
    cat("Species\n")
    table(tm[,speccol])
    cat("DBH range\n")
    print(summary(tm[,dbh]))
    cat("Height\n")
    print(summary(tm[,ht]))
}

## Write data
if (!("temp" %in% list.files()))
    dir.create("temp")
write.csv(pp, "./temp/pp.csv", row.names=FALSE)
write.csv(tp, "./temp/transect.csv", row.names=FALSE)
cat("\n\n\t\t*** Data saved to './temp/transect.csv' and './temp/pp.csv'. ***\n\n")

## tst %>% group_by(ASPCL, ELEVCL) %>%
##     summarise("86"=sum(bv86, na.rm=T),
##               "87" = sum(bv87, na.rm=T),
##               "98"=sum(bv98, na.rm=T),
##               "10"=sum(bv10, na.rm=T))

## res <- tst %>% group_by(ASPCL, ELEVCL) %>%
##     summarise("bv86"=sum(bv86, na.rm=T),
##               "bv87" = sum(bv87, na.rm=T),
##               "bv98"=sum(bv98, na.rm=T),
##               "bv10"=sum(bv10, na.rm=T),
##               "ht86"=sum(ht86*(DBH86/100)**2, na.rm=T),
##               "ht87"=sum(ht87*(DBH87/100)**2, na.rm=T),
##               "ht98"=sum(ht98*(DBH98/100)**2, na.rm=T),
##               "ht10"=sum(ht10*(DBH10/100)**2, na.rm=T))
