### gomp.R --- 
## Filename: gomp.R
## Description: Gompertz
## Author: Noah Peart
## Created: Wed Mar  4 12:52:06 2015 (-0500)
## Last-Updated: Wed Mar  4 15:27:22 2015 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/read/read-moose.R")

library(bbmle)
library(dplyr)
library(magrittr)

## Live ABBAs from 1998
dat <- pp %>% filter(STAT98 == "ALIVE", !is.na(DBH98), SPEC == "ABBA", !is.na(HTTCR98)) %>%
    select(PPLOT, ELEVCL, ASPCL, ends_with("98"), ELEV)
names(dat) <- tolower(names(dat))

# log likelihood function
normNLL <- function(params, x, dbh, elev) {
    sd = params[["sd"]]
    mu = do.call(gompertz, list(params, dbh, elev))
    -sum(dnorm(x, mean = mu, sd = sd, log = TRUE))
}

## Gompertz allometry model
gompertz <- function(ps, dbh, elev) {
    a = ps[["a"]]
    b = ps[["b"]]
    ap = ps[["ap"]]
    bp = ps[["bp"]]
    gamma = ps[["gamma"]]  # could set to 1.37?  but doesn't work quite as well
    (a + b*elev)*exp(log(gamma/(a+b*elev))*exp(-(ap+bp*elev)*dbh))
}

## MLE fit, have some saved pars, otherwise probably need to start with SANN
ps <- readRDS("gomp_pars.rds")
parnames(normNLL) <- c(names(ps))
method <- "Nelder-Mead"  # start with "SANN" if bad initial guesses
maxit <- 100000
summary(fit2 <- mle2(normNLL,
             start = unlist(ps,recursive = FALSE),
             data = list(x = dat[,"httcr98"], dbh=dat[,"dbh98"], elev=dat[,"elev"]),
             method = method,
             control = list(maxit = maxit)))
logLik(fit2)

## save parameters
ps <- as.list(coef(fit2))
saveRDS(ps, file="gomp_pars.rds")

################################################################################
##
##                              Visualize fits
##
################################################################################
library(rgl)

## Points and predicted points
preds <- gompertz(ps, dat$dbh98, dat$elev)
plot3d(xyz.coords(dat$dbh98, dat$elev, preds),
       xlab = "dbh98", ylab = "elev", zlab = "height98",
       main = "Gompertz allometric model predictions")
points3d(xyz.coords(dat$dbh98, dat$elev, dat$httcr98), col = "green")

## residuals
resids <- dat$httcr98 - preds
plot3d(xyz.coords(dat$dbh98, dat$elev, resids),
       xlab = "dbh98", ylab = "elev", zlab = "residuals",
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
