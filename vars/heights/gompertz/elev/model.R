### model.R --- 
## Filename: model.R
## Description: Gompertz allometric model with only elevation (no canopy)
## Author: Noah Peart
## Created: Wed Mar 11 18:09:18 2015 (-0400)
## Last-Updated: Wed Mar 11 22:46:27 2015 (-0400)
##           By: Noah Peart
######################################################################
library(bbmle)

# log likelihood function
normNLL <- function(params, x, dbh, elev) {
    sd = params[["sd"]]
    mu = do.call(gompertz, list(params, dbh, elev))
    -sum(dnorm(x, mean = mu, sd = sd, log = TRUE))
}

## Gompertz allometry model
## beta = (a + b*elev) is asymptote (limit as dbh -> oo)
## gamma = intercept (limit as dbh -> 0)
gompertz <- function(ps, dbh, elev) {
    a = ps[["a"]]
    b = ps[["b"]]
    ap = ps[["ap"]]
    bp = ps[["bp"]]
    gamma <- 1.37
    ## gamma = ps[["gamma"]]  # could set to 1.37?  but doesn't work quite as well
    (a + b*elev)*exp(log(gamma/(a+b*elev))*exp(-(ap+bp*elev)*dbh))
}

## Probably need to run once with simulated annealing to get some reasonable
## parameters, then polish off with nelder-mead if necessary
run_fit <- function(dat, ps, yr, method="Nelder-Mead", maxit=1e5) {
    require(bbmle)
    parnames(normNLL) <- c(names(ps))
    ht <- paste0("HTTCR", yr)
    dbh <- paste0("DBH", yr)
    summary(fit <- mle2(normNLL,
                        start = unlist(ps,recursive = FALSE),
                        data = list(x = dat[, ht], dbh=dat[, dbh], elev=dat[, "ELEV"]),
                        method = method,
                        control = list(maxit = maxit)))
    logLik(fit)
    return( fit )
}
