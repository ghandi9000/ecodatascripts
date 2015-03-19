### visualize_fits.R --- 
## Filename: visualize_fits.R
## Description: Visuals for HH transect fits
## Author: Noah Peart
## Created: Thu Mar 19 18:32:00 2015 (-0400)
## Last-Updated: Thu Mar 19 19:25:26 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/vars/heights/prep.R")  # preps data, adds canhts
source("~/work/ecodatascripts/vars/heights/gompertz/can/model.R")
library(rgl)

## These are just slightly modified from the other visualize_fits.R,
## should probably be combined
get_preds <- function(spec, yrs, base_dir) {
    preds <- lapply(yrs, FUN = function(yr){
        pars <- readRDS(paste0(base_dir, tolower(spec), "_", yr, ".rds"))
        stat <- paste0("STAT", yr)
        dbh <- paste0("DBH", yr)
        ht <- paste0("HT", yr)
        dat <- prep_hh(tp, yr=yr, spec=toupper(spec))
        keep_cols <- c("TPLOT", "ELEV", "ASPCL", "ELEVCL")
        pred <- do.call("gompertz", list(pars, dat[,dbh], dat[,"canht"]))
        res <- cbind(dat[,c(keep_cols, dbh, ht)], pred=pred)
        names(res) <- gsub("[[:digit:]]", "", tolower(names(res)))  # don't track yrs here
        attr(res, "ps") <- pars
        attr(res, "yr") <- yr
        attr(res, "spec") <- spec
        res
    })
    names(preds) <- lapply(yrs, FUN=function(yr) paste0(spec, yr))
    preds
}

plot_preds <- function(preds) {
    ## Plots predicted points
    require(rgl)
    dat <- preds[[1]]
    cols <- palette()[1:length(preds)]
    spec <- attr(dat, "spec")
    
    plot3d(xyz.coords(dat[, "dbh"], dat[, "elev"], dat[, "pred"]),
           xlab = "DBH", ylab = "Elevation", zlab = "Height", col=cols[1],
           main = paste("Gompertz allometric model predictions for HH", spec))
    
    if (length(preds) > 1) {
        for (i in 2:length(preds)) {
            dat <- preds[[i]]
            points3d(xyz.coords(dat[, "dbh"], dat[, "elev"], dat[, "pred"]), col=cols[i])
        }
    }
    yrs <- lapply(preds, FUN = function(x) attr(x, "yr"))
    legend3d("topleft", legend=yrs, text.col=cols)
}

################################################################################
##
##                                   Abbas
##
################################################################################
base_dir <- "~/work/ecodatascripts/vars/heights/gompertz/can/abba/"
yrs <- c(99, 11)
spec <- "ABBA"
preds <- get_preds(spec, yrs, base_dir)

################################################################################
##
##                           Save temporary files
##
################################################################################
temp_dir = "~/work/temp/"
setwd(temp_dir)
plot_preds(preds)
writeWebGL(dir=paste0(temp_dir, "abbaHH"))

library(ggplot2)
dat <- rbind(preds[[1]], preds[[2]]) %>%
    mutate(yr = factor(c(rep(99, nrow(preds[[1]])), rep(11, nrow(preds[[2]])))))
ggplot(dat, aes(dbh, ht, col = yr)) + geom_point(alpha=0.5) + facet_wrap(~ tplot) +
    geom_line(aes(dbh, pred, col = yr)) + ggtitle("HH ABBAs gompertz fits for 99, 11")
dev.off()
ggsave(file="abbaHH.pdf")

################################################################################
##
##                                   Becos
##
################################################################################
base_dir <- "~/work/ecodatascripts/vars/heights/gompertz/can/beco/"
yrs <- c(99, 11)
spec <- "BECO"
preds <- get_preds(spec, yrs, base_dir)

################################################################################
##
##                           Save temporary files
##
################################################################################
temp_dir = "~/work/temp/"
setwd(temp_dir)
plot_preds(preds)
writeWebGL(dir=paste0(temp_dir, "becoHH"))

library(ggplot2)
dat <- rbind(preds[[1]], preds[[2]]) %>%
    mutate(yr = factor(c(rep(99, nrow(preds[[1]])), rep(11, nrow(preds[[2]])))))
ggplot(dat, aes(dbh, ht, col = yr)) + geom_point(alpha=0.5) + facet_wrap(~ tplot) +
    geom_line(aes(dbh, pred, col = yr)) + ggtitle("HH BECOs gompertz fits for 99, 11")
dev.off()
ggsave(file="becoHH.pdf")
