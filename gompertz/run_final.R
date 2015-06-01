### run_final.R --- 
## Filename: run_final.R
## Description: Run full bootstrap with final model
## Author: Noah Peart
## Created: Mon Jun  1 10:14:36 2015 (-0400)
## Last-Updated: Mon Jun  1 17:32:21 2015 (-0400)
##           By: Noah Peart
######################################################################
library(ggplot2)
library(MASS)
library(reshape2)
library(dplyr)
source("~/work/allometry/gompertz/setup.R", chdir=T)

## canopy height in 3x3 including target
## residual elevation from elev ~ canht (in setup.R)
pp$canht <- pp$cht98

## Initialize parameters for no size classes
res <- run_boot(pp, inds=list(1:nrow(pp)), ps=ps, reps=10, ht="HTTCR98", update=TRUE)
ps <- as.list(colMeans(res))

################################################################################
##
##                               Initial model
##
################################################################################
fit <- run_fit(pp, ps, 98, method="Nelder-Mead", dbh="DBH98", height="HTTCR98", canht="canht", elev="relev")
summary(fit)
ps <- as.list(coef(fit))

preds <- do.call(gompertz, list(ps, pp[,"DBH98"], pp[,"relev"], pp[,"canht"]))
rmse <- sqrt( mean( (preds - pp[,"HTTCR98"])^2 ) )

## variance as function of dbh
vps <- ps
vps$c <- 0.1
vps$c1 <- 0.001
vps$c2 <- 0.01

vps$c1 <- NULL
fit2 <- run_fit_var(pp, ps=vps, yr=98)
vps <- as.list(coef(fit2))

preds2 <- do.call(gompertz, list(vps, pp[,"DBH98"], pp[,"relev"], pp[,"canht"]))
rmse2 <- sqrt( mean( (preds2 - pp[,"HTTCR98"])^2 ) )

saveRDS(vps, "~/work/allometry/gompertz/temp/vps.rds")

## Plots
par(mfrow = c(1, 2))
plot(preds2, pp$HTTCR98 - preds2)
plot(preds, pp$HTTCR98 - preds)

plot(HTTCR98 ~ DBH98, data=pp)
points(pp$DBH98, preds, col="blue")
points(pp$DBH98, preds2, col="red")

################################################################################
##
##                                 Bootstrap
##
################################################################################
## Seed with initial model parameters each run
## 8 size classes: 500 bootstraps each, n=1960
R <- 500
n <- 1960
bins <- 8

res <- lapply(1:bins, function(i) {
    pp$qs <- cut(pp[, "DBH98"], breaks=seq(-0.01, max(pp[, "DBH98"])+0.1,
                                      len=i+1))
    inds <- lapply(names(table(pp$qs)), function(x) which(pp$qs == x))
    run_boot(dat=pp, inds=inds, n=n, ps=ps, reps=R)
})

## Root mean squares
rmses <- lapply(res, function(x) {
    sapply(1:nrow(x), function(y) {
        pars <- as.list(x[y, ])
        preds <- do.call(gompertz, list(pars, pp[,"DBH98"], pp[,"relev"], pp[,"canht"]))
        rmse <- sqrt( mean( (preds - pp[,"HTTCR98"])^2 ) )
    })
})

res <- melt(do.call(rbind, res), varnames=c("bins", "coef"))
res$bins <- as.factor(rep(rep(1:bins, each=R+1), times=length(names(table(res$coef)))))
res$run <- as.factor(rep(rep(1:(R+1), times=length(names(table(res$coef)))), times=bins))
attr(res, "rmse") <- do.call(c, rmses)
attr(res, "bin") <- as.factor(rep(1:bins, each=R+1))
attr(res, "run") <- as.factor(rep(1:(R+1), times=bins))

saveRDS(res, "~/work/allometry/gompertz/temp/boot_results.rds")

