### errors.R --- 
## Filename: errors.R
## Description: 
## Author: Noah Peart
## Created: Tue May 19 11:22:39 2015 (-0400)
## Last-Updated: Thu May 21 20:44:55 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/allometry/gompertz/model.R")
pp <- read.csv("~/work/treedata/pp.csv")

## local canopies -- follow zach's subsetting procedure and local canopy construction
pp <- pp[!is.na(pp$HTTCR98) & pp$HTTCR98 > 0, ]
pp$cht98 <- NA
for (i in 1:nrow(pp))
    pp$cht98[i] <- max(pp[pp$BQUDX >= pp$BQUDX[i]-1 & pp$BQUDY >= pp$BQUDY[i]-1 &
                              pp$BQUDX <= pp$BQUDX[i]+1 & pp$BQUDY <= pp$BQUDY[i]+1 & pp$PPLOT == pp$PPLOT[i],
                          "HTTCR98"])
pp <- pp[!is.na(pp$DBH98) & pp$DBH98 > 0 & !is.na(pp$cht98) & pp$BQUDX > 1 & pp$BQUDX < 10 &
             pp$BQUDY > 1 & pp$BQUDY < 10, ]
pp <- pp[pp$SPEC == "ABBA", ]

## Bootstrap for 98 data, sampling equally from quantiles of DBH98
yr <- 98
stat <- paste0("STAT", yr)
dbh <- paste0("DBH", yr)
ht <- paste0("HTTCR", yr)
canht <- paste0("cht", yr)
dat <- pp[pp[,stat] == "ALIVE" & pp$SPEC == "ABBA" & !is.na(pp[,dbh]) &
             !is.na(pp[,ht]), ]
ps <- readRDS("~/work\\ecodatascripts\\vars\\heights\\gompertz\\full\\abba\\abba_98.rds")

## Fitting parameters
reps <- 20         # number of bootstraps
nqs <- 5           # number of quantiles (may be shortened if quantiles overlap)
p <- 0.1           # % of smallest quantile to sample
showPlot <- TRUE   # show plot as fitting
showRes <- TRUE    # show residuals

## construct quantiles
qs <- unique(quantile(dat[,dbh], probs = seq(0, 1, 1/nqs)))
dat$qs <- cut(dat[,dbh], qs, include.lowest = TRUE)
nqs <- length(names(table(dat$qs)))
inds <- lapply(names(table(dat$qs)), function(x) which(dat$qs == x))
n <- floor(min(p*unlist(lapply(inds, length))))  # num. samples/quantile

## Run bootstrap
if (showPlot && showRes) {
    dev.new()
    plot(dat[,dbh], dat[,ht], type="n", xlab=dbh, ylab=ht)
    abline(v = qs, lty=2, lwd=2)
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

res <- unlist(ps)
for (i in 1:reps) {
    ii <- unlist(lapply(inds, function(x) sample(x, n, replace=T)))
    samp <- dat[ii,]
    points(samp[,dbh], samp[,ht], col = i)
    fit <- NULL
    ## try({
    ## form <- as.formula(paste(ht, "~", "a*", dbh, "^b"))
    ## fit <- nls(HTTCR98 ~ a * DBH98 ^ b, start=list(a=0.5, b=0.1), data=samp)
    fit <- run_fit(samp, ps, 98, method="SANN", maxit=2e6)
    params <- coef(fit)
    ## }, silent = TRUE)
    if (!is.null(fit)) {
        res <- rbind(res, unlist(params))
        if (showPlot || showRes) {
            ## preds <- ps[1] * dd[,"DBH98"]^ps[2]
            preds <- do.call(gompertz, list(ps=params, dbh=samp[,dbh], elev=samp[,"ELEV"], 
                                            canht=samp[,canht]))
            iis <- sort(samp[,dbh], index.return=TRUE)
            if (showPlot && showRes) {
                points(iis$x, preds[iis$ix], col=i, type="l")
                dev.set(dev.next())
                points(preds[iis$ix], samp[,ht] - preds[iis$ix])
                dev.set(dev.prev())
            } else if (showPlot)
                points(iis$x, preds[iis$ix], col=i, type="l")
              else if (showRes)
                  points(preds[iis$ix], samp[,ht] - preds[iis$ix], col=i, pch=16, alpha=0.5)
        }
    }
}

## std. error of estimates
stdErr <- matrix(sqrt(diag(cov(res)) / nrow(res)))
bias <- matrix(colSums(res - colMeans(res)) / nrow(res))
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
