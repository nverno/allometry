### errors.R --- 
## Filename: errors.R
## Description: 
## Author: Noah Peart
## Created: Tue May 19 11:22:39 2015 (-0400)
## Last-Updated: Wed May 20 22:58:44 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/allometry/gompertz/model.R")
pp <- read.csv("~/work/treedata/pp.csv")

## Bootstrap for 98 data, sampling equally from quantiles of DBH98
yr <- 98
stat <- paste0("STAT", yr)
dbh <- paste0("DBH", yr)
ht <- paste0("HTTCR", yr)
canht <- paste0("canht", yr)
dat <- pp[pp[,stat] == "ALIVE" & pp$SPEC == "ABBA" & !is.na(pp[,dbh]) &
             !is.na(pp[,ht]), ]
ps <- readRDS("~/work\\ecodatascripts\\vars\\heights\\gompertz\\full\\abba\\abba_98.rds")

## Fitting parameters
reps <- 30        # number of bootstraps
nqs <- 40         # number of quantiles (may be shortened if quantiles overlap)
p <- 0.4          # % of smallest quantile to sample
showPlot <- TRUE  # show plot as fitting
showRes <- TRUE   # show residuals

## construct quantiles
qs <- unique(quantile(dat[,dbh], probs = seq(0, 1, 1/nqs)))
nqs <- length(qs)
dat$qs <- cut(dat[,dbh], qs, include.lowest = TRUE)
inds <- lapply(names(table(dat$qs)), function(x) which(dat$qs == x))
n <- floor(min(p*unlist(lapply(inds, length))))
    
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

res <- ps
for (i in 1:reps) {
    ii <- unlist(lapply(inds, function(x) sample(x, n, replace=T)))
    samp <- dat[ii,]
    points(samp[,dbh], samp[,ht], col = i)
    fit <- NULL
    ## try({
    ## form <- as.formula(paste(ht, "~", "a*", dbh, "^b"))
    ## fit <- nls(HTTCR98 ~ a * DBH98 ^ b, start=list(a=0.5, b=0.1), data=samp)
    fit <- run_fit(samp, ps, 98, method="SANN", maxit=10e7)
    ps <- coef(fit)
    ## }, silent = TRUE)
    if (!is.null(fit)) {
        res <- rbind(res, ps)
        if (showPlot || showRes) {
            ## preds <- ps[1] * dd[,"DBH98"]^ps[2]
            preds <- do.call(gompertz, list(ps=ps, dbh=samp[,dbh], elev=samp[,"ELEV"], 
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

while(dev.cur())
    dev.off()

pp <- pp[pp$SPEC == "ABBA" & pp$STAT98 == "ALIVE", !is.na(pp$DBH98) & !is.na(pp$HTTCR98), ]
## plot(pp$ELEV, pp$HTTCR98)
## ps <- coef(fit)
## preds <- do.call(gompertz, list(ps=ps, dbh=pp[,"DBH98"], elev=pp[,"ELEV"], canht=pp[,"canht98"]))
## inds <- sort(pp$ELEV , index.return=T)
## points(pp$ELEV, preds, col="red", type="p")
## ## lines(inds$x, preds[inds$ix], col="red")
## boxplot(preds ~ pp$qcan)

## pp$qcan <- cut(pp$canht98, quantile(pp$canht98, probs=seq(0, 1, 1/10), na.rm=T, include.lowest = TRUE))

abbas <- pp[pp$SPEC=="ABBA" & pp$STAT98=="ALIVE" & !is.na(pp$DBH98) & !is.na(pp$HTTCR98),]
ps <- coef(fit)
preds <- do.call(gompertz, list(ps=ps, dbh=abbas[,"DBH98"], elev=abbas[,"ELEV"], canht=abbas[,"canht98"]))
inds <- sort(abbas$DBH98 , index.return=T)

par(mfrow = c(2,1))
plot(abbas$DBH98, abbas$HTTCR98, main = "HT vs DBH (1998) with Gompertz predictions",
     xlab="DBH98", ylab="HT98")
legend("topleft", col="red", "Predicted", pch=1)
points(abbas$DBH98, preds, col="red", type="p")

plot(preds, abbas$HTTCR98 - preds, main="Residuals", xlab="Predicted", ylab="Observed - Predicted")
abline(h=0, lty=2)
