### visualize_fits.R --- 
## Filename: visualize_fits.R
## Description: Some visuals for gompertz fits
## Author: Noah Peart
## Created: Wed Mar 11 20:33:24 2015 (-0400)
## Last-Updated: Wed Mar 25 18:20:03 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/read/read-moose.R")
source("~/work/ecodatascripts/read/read-transect.R")
source("~/work/ecodatascripts/vars/heights/prep.R")  # preps data, adds canhts
source("~/work/ecodatascripts/vars/heights/vis_functions.R")  # helpers for 3D visuals

## Gompertz models
## source("~/work/ecodatascripts/vars/heights/gompertz/elev/model.R")
## source("~/work/ecodatascripts/vars/heights/gompertz/full/model.R")
## Neg-exp models
## source("~/work/ecodatascripts/vars/heights/negexp/full/model.R")
## source("~/work/ecodatascripts/vars/heights/negexp/elev/model.R")

library(rgl)
library(plyr)
library(dplyr)

################################################################################
##
##                                 Visualize
##
################################################################################
## 3D
spec <- "betula"
model <- "negexp"
inds <- "full"
hh <- FALSE
years <- c(86)
preds <- get_preds(spec, years, modtype = model, inds = inds, hh=hh)
plot_preds(preds)
add_pred_lines(preds)
add_observed(preds)

## residuals
dat <- preds[[1]]
resids <- dat$httcr - dat$pred
plot3d(xyz.coords(dat$dbh, dat$elev, resids),
       xlab = "dbh", ylab = "elev", zlab = "residuals",
       main = "Gompertz allometric model residuals")
planes3d(c(0,0,1), alpha = 0.2, col = "red")

## Surface
x <- with(dat, seq(min(dbh98), max(dbh98), length=50))
y <- with(dat, seq(min(elev), max(elev), length=20))
z <- outer(x, y, gompertz, ps=ps)
surface3d(x, y, z, col = "green", alpha = 0.5, front = "line", back = "line")
points3d(xyz.coords(dat$dbh98, dat$elev, dat$httcr98), col = "red")
axes3d()
decorate3d(xlab = "dbh98", ylab="elev", zlab="height98", main = "Predicted surface, observed points")

## 2D
dat <- preds[[1]]
library(ggplot2)
ggplot(dat, aes(dbh, ht)) + geom_point(alpha=0.5) + facet_wrap(~canht) +
    geom_line(aes(dbh, pred))

## For hh
dat <- rbind(preds[[1]], preds[[2]]) %>%
    mutate(yr = factor(c(rep(99, nrow(preds[[1]])), rep(11, nrow(preds[[2]])))))
ggplot(dat, aes(dbh, ht, col = yr)) + geom_point(alpha=0.5) + facet_wrap(~ canht) +
    geom_line(aes(dbh, pred, col = yr)) + ggtitle(paste("HH", spec, "gompertz fits for 99, 11"))

################################################################################
##
##                           Save temporary files
##
################################################################################
temp_dir = "~/work/temp/"
setwd(temp_dir)
plot_preds(preds)
writeWebGL(dir=paste0(temp_dir, "abbaHH"))

dat <- rbind(preds[[1]], preds[[2]]) %>%
    mutate(yr = factor(c(rep(99, nrow(preds[[1]])), rep(11, nrow(preds[[2]])))))
ggplot(dat, aes(dbh, ht, col = yr)) + geom_point(alpha=0.5) + facet_wrap(~ tplot) +
    geom_line(aes(dbh, pred, col = yr)) + ggtitle("HH ABBAs gompertz fits for 99, 11")
dev.off()
ggsave(file="abbaHH.pdf")


