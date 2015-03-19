### model.R --- 
## Filename: model.R
## Description: Gompertz allometric model with only canopy
## Author: Noah Peart
## Created: Wed Mar 11 18:09:18 2015 (-0400)
## Last-Updated: Thu Mar 19 17:34:57 2015 (-0400)
##           By: Noah Peart
######################################################################
library(bbmle)

# log likelihood function
normNLL <- function(params, x, dbh, canht) {
    sd = params[["sd"]]
    mu = do.call(gompertz, list(params, dbh, canht))
    -sum(dnorm(x, mean = mu, sd = sd, log = TRUE))
}

## Gompertz allometry model with only canopy height
## beta = (a + b*cant) is asymptote (limit as dbh -> oo)
## gamma = intercept (limit as dbh -> 0)
gompertz <- function(ps, dbh, canht) {
    a = ps[["a"]]
    b = ps[["b"]]
    ap = ps[["ap"]]
    bp = ps[["bp"]]
    gamma <- 1.37
    (a + b*canht)*exp(log(gamma/(a+b*canht))*exp(-(ap+bp*canht)*dbh))
}

## Probably need to run once with simulated annealing to get some reasonable
## parameters, then polish off with nelder-mead if necessary
run_fit <- function(dat, ps, yr, method="Nelder-Mead", maxit=1e5) {
    require(bbmle)
    parnames(normNLL) <- c(names(ps))
    ht <- paste0("HT", yr)
    dbh <- paste0("DBH", yr)
    summary(fit <- mle2(normNLL,
                        start = unlist(ps,recursive = FALSE),
                        data = list(x = dat[, ht], dbh=dat[, dbh], canht=dat[, "canht"]),
                        method = method,
                        control = list(maxit = maxit)))
    logLik(fit)
    return( fit )
}
