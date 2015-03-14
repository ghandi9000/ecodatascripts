### model.R --- 
## Filename: model.R
## Description: Negative exponential allometric model elevation and canopy
## Author: Noah Peart
## Created: Wed Mar 11 18:09:18 2015 (-0400)
## Last-Updated: Fri Mar 13 23:34:37 2015 (-0400)
##           By: Noah Peart
######################################################################
library(bbmle)

# log likelihood function
normNLL <- function(params, x, dbh, elev, canht) {
    sd = params[["sd"]]
    mu = do.call(negexp, list(params, dbh, elev, canht))
    -sum(dnorm(x, mean = mu, sd = sd, log = TRUE))
}

## Negative exponentil allometry model with only elevation
## H = gamma + (beta - gamma)(1 - e^(-alpha*D))
## beta = (a + b*elev) is asymptote (limit as dbh -> oo)
## alpha = (ap + bp*elev)
## gamma = intercept (limit as dbh -> 0)
negexp <- function(ps, dbh, elev, canht) {
    a = ps[["a"]]
    a1 = ps[["a1"]]
    a2 = ps[["a2"]]        
    a3 = ps[["a3"]]        
    b = ps[["b"]]
    b1 = ps[["b1"]]
    b2 = ps[["b2"]]        
    b3 = ps[["b3"]]        
    gamma <- 1.37  # set to DBH height
    alpha <- a + a1*elev + a2*canht + a3*elev*canht
    beta <- b + b1*elev + b2*canht + b3*elev*canht

    gamma + (beta - gamma)*(1 - exp(-alpha * dbh))
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
                        data = list(x = dat[, ht], dbh=dat[, dbh], elev=dat[, "ELEV"],
                                    canht=dat[, "canht"]),
                        method = method,
                        control = list(maxit = maxit)))
    logLik(fit)
    return( fit )
}
