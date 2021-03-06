### model.R --- 
## Filename: model.R
## Description: 
## Author: Noah Peart
## Created: Tue May 19 11:24:15 2015 (-0400)
## Last-Updated: Mon Jun  1 17:32:28 2015 (-0400)
##           By: Noah Peart
######################################################################
library(bbmle)

## Normal log likelihood function
normNLL <- function(params, x, dbh, elev, canht) {
    sd = params[["sd"]]
    mu = do.call(gompertz, list(params, dbh, elev, canht))
    -sum(dnorm(x, mean = mu, sd = sd, log = TRUE))
}

## Gompertz allometry model
## beta = a + a1*elev + a2*canopy + a3*elev*canopy (limit as dbh -> oo)
## alpha = b + b1*elev + b2*canopy + b3*elev*canopy
## gamma = intercept (limit as dbh -> 0)  # set to DBH height = 1.37 meters
gompertz <- function(ps, dbh, elev, canht) {
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

    beta*exp( log(gamma/beta)*exp( -alpha*dbh ) )
}

## run model
run_fit <- function(dat, ps, yr, method="Nelder-Mead", maxit=1e5, height="HTTCR98",
                    dbh="DBH98", canht="canht", elev="relev", ...) {
    require(bbmle)
    parnames(normNLL) <- c(names(ps))
    ## ht <- paste0(height, yr)
    ## dbh <- paste0(dbh, yr)
    ## canht <- paste0(canht, yr)
    fit <- mle2(normNLL,
                start = unlist(ps,recursive = FALSE),
                data = list(x = dat[, height], dbh=dat[, dbh], elev=dat[, elev],
                canht=dat[,canht]),
                method = method,
                control = list(maxit = maxit))
    return( fit )
}

## Normal log likelihood function with sd function
normNLL_var <- function(params, x, dbh, elev, canht) {
    c = params[["c"]]
    ## c1 = params[["c1"]]
    c2 = params[["c2"]]
    sd = params[["sd"]]
    mu = do.call(gompertz, list(params, dbh, elev, canht))
    sd = c*dbh + c2*canht + sd
    -sum(dnorm(x, mean = mu, sd = sd, log = TRUE))
}

## model with variance as function of variables
run_fit_var <- function(dat, ps, yr, method="Nelder-Mead", maxit=1e5, height="HTTCR98",
                        dbh="DBH98", canht="canht", elev="relev", ...) {
    require(bbmle)
    parnames(normNLL_var) <- c(names(ps))
    ## ht <- paste0(height, yr)
    ## dbh <- paste0(dbh, yr)
    ## canht <- paste0(canht, yr)
    fit <- mle2(normNLL_var,
                start = unlist(ps,recursive = FALSE),
                data = list(x = dat[, height], dbh=dat[, dbh], elev=dat[, elev],
                    canht=dat[,canht]),
                method = method,
                control = list(maxit = maxit))
    return( fit )
}

