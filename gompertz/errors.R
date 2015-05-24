### errors.R --- 
## Filename: errors.R
## Description: Bootstrap gompertz fits with even DBH distributions
## Author: Noah Peart
## Created: Tue May 19 11:22:39 2015 (-0400)
## Last-Updated: Sat May 23 18:05:47 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/allometry/gompertz/model.R")
## source("~/work/allometry/gompertz/zach_setup.R")
pp <- readRDS("~/work/allometry/gompertz/temp/pp.rds")
res <- lm(ELEV ~ cht98, data=pp)
pp$relev <- residuals(res)

## Bootstrap for 98 data, sampling equally from quantiles of DBH98
yr <- 98
stat <- paste0("STAT", yr)
dbh <- paste0("DBH", yr)
ht <- paste0("HTTCR", yr)
canht <- paste0("cht", yr)
elev <- "relev"
dat <- pp[pp[,stat] == "ALIVE" & pp$SPEC == "ABBA" & !is.na(pp[,dbh]) &
             !is.na(pp[,ht]), ]
## ps <- readRDS("~/work\\ecodatascripts\\vars\\heights\\gompertz\\full\\abba\\abba_98.rds")
## Parameters from manuscript
ps <- list(a=0.199, a1=0.000251, a2=0.00511, a3=-0.0000141, b=2.31, b1=0.00410, b2=0.92, b3=-0.000271,
           sd=1)

## Fitting parameters
reps <- 150         # number of bootstraps
nqs <- 5           # number of quantiles (may be shortened if quantiles overlap)
p <- 0.5           # % of smallest quantile to sample
showPlot <- TRUE   # show plot as fitting
showRes <- TRUE    # show residuals

## construct quantiles
qs <- unique(quantile(dat[,dbh], probs = seq(0, 1, 1/nqs)))
## dat$qs <- cut(dat[,dbh], qs, include.lowest = TRUE)
breaks <- c(-1, 5, 15, 50)
dat$qs <- cut(dat[,dbh], breaks=breaks)
nqs <- length(names(table(dat$qs)))
inds <- lapply(names(table(dat$qs)), function(x) which(dat$qs == x))
n <- floor(min(p*unlist(lapply(inds, length))))  # num. samples/quantile

## Run bootstrap
if (showPlot && showRes) {
    dev.new()
    plot(dat[,dbh], dat[,ht], type="n", xlab=dbh, ylab=ht)
    abline(v = breaks, lty=2, lwd=2)
    dev.new()
    plot(0, 0, ylim=c(-6,6),  xlim=c(min(dat[,ht]),max(dat[,ht])), type='n', main="Residuals")
    abline(h=0, lty=2)
    dev.set(dev.prev())
} else if (showPlot) {
    plot(dat[,dbh], dat[,ht], type="n", xlab=dbh, ylab=ht)
} else if (showRes) {
    plot(0, 0, ylim=c(-6,6), xlim=c(min(dat[,ht]),max(dat[,ht])), type='n', main="Residuals")
    abline(h=0, lty=2)
}

best_fit <- NULL
best_aic <- NULL
res <- unlist(ps)
for (i in 1:reps) {
    ii <- unlist(lapply(inds, function(x) sample(x, n, replace=T)))
    samp <- dat[ii,]
    points(samp[,dbh], samp[,ht], col=i)
    ## try({
    ## form <- as.formula(paste(ht, "~", "a*", dbh, "^b"))
    ## fit <- nls(HTTCR98 ~ a * DBH98 ^ b, start=list(a=0.5, b=0.1), data=samp)
    if (i %% 50 == 0) {
        cat(paste("\nFitting with Simulated Annealing: iteration", i, "\nCurrent AIC(sample):", 
                  AIC(fit), "\n"))
        fit <- run_fit(samp, ps, 98, method="SANN", maxit=2e6)
    }
    else
        fit <- run_fit(samp, ps, 98, method="Nelder-Mead")
    params <- coef(fit)
    ## }, silent = TRUE)
    if (!is.null(fit)) {
        res <- rbind(res, unlist(params))
        if (showPlot || showRes) {
            ## preds <- ps[1] * dd[,"DBH98"]^ps[2]
            preds <- do.call(gompertz, list(ps=params, dbh=samp[,dbh], elev=samp[,elev], 
                                            canht=samp[,canht]))
            iis <- sort(samp[,dbh], index.return=TRUE)
            if (showPlot && showRes) {
                points(iis$x, preds[iis$ix], col=i, type="l")
                dev.set(dev.next())
                points(preds, samp[,ht]-preds, col=i)
                dev.set(dev.prev())
            } else if (showPlot)
                points(iis$x, preds[iis$ix], col=i, type="l")
              else if (showRes)
                  points(preds, samp[,ht]-preds, col=i, pch=16, alpha=0.5)
        }
    }
    ## identify(samp[,dbh], samp[,ht], labels=round(samp$cht98, 2))
    ps <- params
    if (i %% 5) {
        fitAll <- run_fit(pp, ps, 98)
        aic <- AIC(fitAll)
        if (is.null(best_fit) || aic < best_aic) {
            best_aic <- aic
            best_fit <- fitAll
            cat(paste("\nNew best AIC:", best_aic, "\n"))
        }
        ps <- coef(fitAll)
    }
}

## std. error of estimates
ests <- colMeans(res)
fit <- run_fit(dat, ps, 98, method="Nelder-Mead")
stdErr <- matrix(sqrt(diag(cov(res)) / nrow(res)))
as.matrix(data.frame(Estimate=ests, Std=stdErr, Cov=stdErr/colMeans(res)*100))

## bias <- matrix(colSums(res - colMeans(res)) / nrow(res))
while(dev.cur())
    dev.off()

## pp <- pp[pp$SPEC == "ABBA" & pp$STAT98 == "ALIVE", !is.na(pp$DBH98) & !is.na(pp$HTTCR98), ]
## plot(pp$ELEV, pp$HTTCR98)
## ps <- coef(fit)
## preds <- do.call(gompertz, list(ps=ps, dbh=pp[,"DBH98"], elev=pp[,"ELEV"], canht=pp[,"canht98"]))
## inds <- sort(pp$ELEV , index.return=T)
## points(pp$ELEV, preds, col="red", type="p")
## ## lines(inds$x, preds[inds$ix], col="red")
## boxplot(preds ~ pp$qcan)

## pp$qcan <- cut(pp$canht98, quantile(pp$canht98, probs=seq(0, 1, 1/10), na.rm=T, include.lowest = TRUE))

## abbas <- pp[pp$SPEC=="ABBA" & pp$STAT98=="ALIVE" & !is.na(pp$DBH98) & !is.na(pp$HTTCR98),]
## ps <- coef(fit)
## preds <- do.call(gompertz, list(ps=ps, dbh=abbas[,"DBH98"], elev=abbas[,"ELEV"], canht=abbas[,"canht98"]))
## inds <- sort(abbas$DBH98 , index.return=T)

## par(mfrow = c(2,1))
## plot(abbas$DBH98, abbas$HTTCR98, main = "HT vs DBH (1998) with Gompertz predictions",
##      xlab="DBH98", ylab="HT98")
## legend("topleft", col="red", "Predicted", pch=1)
## points(abbas$DBH98, preds, col="red", type="p")

## plot(preds, abbas$HTTCR98 - preds, main="Residuals", xlab="Predicted", ylab="Observed - Predicted")
## abline(h=0, lty=2)

## Using boot package (basic non-parametric boot)
library(boot)
bootGomp <- function(data, indices, ps, maxit=1e5, yr=98) {
    data <- data[indices, ]
    mod <- run_fit(data, ps=ps, yr=98, method="SANN", maxit=maxit)
    coef(mod)
}

packageRes <- boot(data=dat, statistic=bootGomp, ps=ps, R=100)

pow <- function(x) 2*x^3
x <- 1:100
y <- 1:100
tst <- rnorm(pow, )
