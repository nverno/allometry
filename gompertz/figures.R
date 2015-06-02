### figures.R --- 
## Filename: figures.R
## Description: Figures from bootstrap results
## Author: Noah Peart
## Created: Mon Jun  1 14:48:51 2015 (-0400)
## Last-Updated: Mon Jun  1 22:17:22 2015 (-0400)
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

################################################################################
##
##                                 Analysis
##
################################################################################
res <- readRDS("~/work/allometry/gompertz/temp/boot_results.rds")
AB <- dcast(res, bins + run ~ coef)
R <- 500
n <- 1960
bins <- 8

## Note: Run 1 is seeded parameters
## Trace coefficients
ests <- res %>% filter(run != 1) %>%
  group_by(bins, coef) %>% summarise(estimate=mean(value))

## Trace alpha parameters
pars <- c("a", "a1", "a2", "a3")  # parameter(s) to trace
dat <- res[res$coef %in% pars & res$run != 1, ]

traceA <- ggplot(dat, aes(bins, value, group=bins, color=coef)) +
  theme_bw() + xlab("Number of Size Classes") + ylab("Parameter Estimates") +
  geom_boxplot(alpha=0.5) +
  geom_point(alpha=0.2, size=2, position=position_jitter(width=0.05)) +
  facet_wrap(~coef, scales="free") +
  ggtitle(paste("Bootstrapped estimates(alpha):", bins, "size classes,", R, "boostraps (n = ", n, ")"))
  ## stat_summary(fun.y=mean, geom="point", fill="black", pch=21, size=3) +
  ## stat_summary(fun.data=mean_cl_normal, geom="errorbar", color="black", width=0.25, alpha=0.7,
  ##              conf.int=0.95)
traceA

traceAbox <- ggplot(dat, aes(bins, value, group=bins, color=coef)) +
  theme_bw() + xlab("Number of Size Classes") + ylab("Parameter Estimates") +
  geom_boxplot() + facet_wrap(~coef, scales="free") +
  ggtitle(paste("Bootstrapped estimates(alpha):", bins, "size classes,", R, "boostraps (n = ", n, ")"))
traceAbox

## Trace beta parameters
pars <- c("b", "b1", "b2", "b3")  # parameter(s) to trace
dat <- res[res$coef %in% pars & res$run != 1, ]

traceB <- ggplot(dat, aes(bins, value, group=bins, color=coef)) +
  theme_bw() + xlab("Number of Size Classes") + ylab("Parameter Estimates") +
  geom_boxplot(alpha=0.5) +
  geom_point(alpha=0.2, size=2, position=position_jitter(width=0.05)) +
  facet_wrap(~coef, scales="free") +
  ggtitle(paste("Bootstrapped estimates(beta):", bins, "size classes,", R, "boostraps (n = ", n, ")"))
traceB                 

traceBbox <- ggplot(dat, aes(bins, value, group=bins, color=coef)) +
  theme_bw() + xlab("Number of Size Classes") + ylab("Parameter Estimates") +
  geom_boxplot() + facet_wrap(~coef, scales="free") +
  ggtitle(paste("Bootstrapped estimates(beta):", bins, "size classes,", R, "boostraps (n = ", n, ")"))
traceBbox

## trace sd
pars <- c("sd")  # parameter(s) to trace
dat <- res[res$coef %in% pars & res$run != 1, ]

traceSD <- ggplot(dat, aes(bins, value, group=bins, color=coef)) +
  theme_bw() + xlab("Number of Size Classes") + ylab("Parameter Estimates") +
  geom_boxplot(alpha=0.5) +
  geom_point(alpha=0.2, size=2, position=position_jitter(width=0.05)) +
  facet_wrap(~coef, scales="free") +
  ggtitle(paste("Bootstrapped estimates(sd):", bins, "size classes,", R, "boostraps (n = ", n, ")"))
traceSD

## Trace RMSE
dat <- data.frame(bin=attr(res, "bin"), rmse=attr(res, "rmse"), run=attr(res, "run"))
dat <- dat[dat$run != 1, ]

ggplot(dat, aes(bin, rmse)) + ## geom_point(alpha=0.4, size=3, position=position_jitter(width=0.05)) +
  ggtitle(paste("Bootstrapped estimates(RMSE):", bins, "size classes,", R, "boostraps (n = ", n, ")")) +
  theme_bw() +
  geom_boxplot(alpha=0.5) +
  xlab("Number of Size Classes") + ylab("RMSE (on original data)")
  ## stat_summary(fun.y=mean, geom="point", fill="black", pch=21, size=3) +
  ## stat_summary(fun.data=mean_cl_normal, geom="errorbar", color="black", width=0.25, 
  ##              alpha=0.7, conf.int=0.95)

################################################################################
##
##                                Alpha-Beta
##
################################################################################
## Compute alpha and beta for combinations of canht/elev with conf.int
calpha <- function(bin, elev, canht) {
    with(AB[AB$bins==bin & AB$run != 1, ],  {
        alpha = a + a1*elev + a2*canht + a3*elev*canht
        mean(alpha)
        ## list(alpha=mean(alpha), alpha.conf=quantile(alpha, probs=c(0.025, 0.975)),
        ##      beta=mean(beta), beta.conf=quantile(beta, probs=c(0.025, 0.975)))
    })
}
calpha <- Vectorize(calpha, vectorize.args = c("elev", "canht"))

cbeta <- function(bin, elev, canht) {
    with(AB[AB$bins==bin & AB$run != 1, ],  {
        beta = b + b1*elev + b2*canht + b3*elev*canht
        mean(beta)
        ## list(alpha=mean(alpha), alpha.conf=quantile(alpha, probs=c(0.025, 0.975)),
        ##      beta=mean(beta), beta.conf=quantile(beta, probs=c(0.025, 0.975)))
    })
}
cbeta <- Vectorize(cbeta, vectorize.args = c("elev", "canht"))

## Estimates for manuscript
aANDb <- Vectorize(function(bin, elev, canht) {
    with(AB[AB$bins==bin & AB$run != 1, ],  {
        alpha = a + a1*elev + a2*canht + a3*elev*canht
        beta = b + b1*elev + b2*canht + b3*elev*canht
        alpha.conf=quantile(alpha, probs=c(0.025, 0.975))
        beta.conf=quantile(beta, probs=c(0.025, 0.975))
        data.frame(alpha=unlist(mean(alpha)), alpha.lower=alpha.conf[[1]], alpha.upper=alpha.conf[[2]],
                   beta=unlist(mean(beta)), beta.lower=beta.conf[[1]], beta.upper=beta.conf[[2]],
                   elev=elev, canht=canht)
    })
}, vec=c("elev", "canht"))

## Output tables
library(gridExtra)

elevs <- c(-100, 0, 100)
canhts <- c(9, 12.5, 16)
vars <- expand.grid(elev=elevs, canht=canhts)
bin <- 1
out1 <- as.data.frame(t(aANDb(bin=1, elev=vars$elev, canht=vars$canht)))
out6 <- as.data.frame(t(aANDb(bin=6, elev=vars$elev, canht=vars$canht)))
out1$alpha <- unlist(out1$alpha)
out6$alpha <- unlist(out6$alpha)
out1$beta <- unlist(out1$beta)
out6$beta <- unlist(out6$beta)
dat <- data.frame(calpha=(out1$alpha-out6$alpha)/out1$alpha, cbeta=(out1$beta-out6$beta)/out1$beta, 
                  elev=unlist(out1$elev), canht=unlist(out1$canht))
pdf(file = paste0("change_alpha_beta_bin1_to_6.pdf"))
grid.table(dat, show.rownames=F, digits=2)
dev.off()

alphas1 <- out[, c("alpha", "alpha.lower", "alpha.upper", "elev", "canht")]
pdf(file = paste0("alphas_", bin, "_bin.pdf"))
grid.table(alphas, show.rownames=F, digits=2)
dev.off()

betas <- out[, c("beta", "beta.lower", "beta.upper", "elev", "canht")]
pdf(file = "betas_1_bin.pdf")
grid.table(betas, show.rownames=F, digits=2)
dev.off()

## Combinations of elevation(residual) and canopy height to fix for calculation
elevs <- seq(-100, 100, length=100)
canhts <- seq(min(pp$canht), max(pp$canht), length=100)

## [alpha|beta] ~ elev + canht
bin <- 1
alphaVals <- outer(elevs, canhts, calpha, bin=bin)
betaVals <- outer(elevs, canhts, cbeta, bin=bin)

library(lattice)
wireframe(alphaVals, row.values = elevs, column.values = canhts, 
          zlab="alpha", xlab="Residual Elevation",
          ylab="Canopy Height")

plot(canhts, alphaVals[100,])
plot(canhts, betaVals[50,])
points(canhts, betaVals[1,])
points(canhts, betaVals[100,], col="blue")
abline(0,1)

################################################################################
##
##                 Empirical confidence interval for height
##
################################################################################
predInt <- Vectorize(function(bin, dbh, elev, canht, low=0.025, upper=0.975) {
    with(AB[AB$bins==bin & AB$run != 1,], {
        alpha = a + a1*elev + a2*canht + a3*elev*canht
        beta = b + b1*elev + b2*canht + b3*elev*canht
        ht = beta * exp( log(1.37/beta) * exp( -alpha*dbh ) )
        ht.conf=quantile(ht, probs=c(low, upper))
        c(ht=mean(ht), lower=ht.conf[[1]], upper=ht.conf[[2]])
    })
}, vec="dbh")

bin <- 1
elev <- 0
canht <- 9
dbhs <- seq(0, max(pp$DBH98), length=100)
preds <- predInt(bin, dbhs, 0, canht)

plot(dbhs, preds[1,], type="l")
lines(dbhs, preds[2,])
lines(dbhs, preds[3,])

## fix residual elevation vs canopy height, plot ht vs dbh with empirical confidence
dbhs <- seq(0, max(pp$DBH98), length=100)
elevs <- c(-100, 0, 100)
canhts <- c(9, 12.5, 16)
vars <- expand.grid(elev=elevs, canht=canhts)
bins <- c(1, 6)

par(mfrow = c(2, 3))
for (bin in bins) {
    for (i in seq_along(elevs)) {
        plot(HTTCR98 ~ DBH98, data=pp, type="n", 
             main=paste0("Predicted Height vs. DBH, \n(residual elevation=", elevs[i], 
                 ", Bins=", bin, ")"))
        cols <- c("chartreuse2", "coral1", "cornflowerblue")
        for (j in seq_along(canhts)) {
            preds <- predInt(bin, dbhs, elevs[i], canhts[j])
            lines(dbhs, preds[1,], lwd=2, col=cols[j])
            lines(dbhs, preds[2,], lty=2, col=cols[j])
            lines(dbhs, preds[3,], lty=2, col=cols[j])
        }
        legend("topleft", c(paste("Canopy:", canhts)), col=cols, lty=1, lwd=2)
    }
}

## Confidence intervals in ggplot
dat <- apply(vars, 1, function(x) {
    out <- lapply(bins, function(bin) {
        t(predInt(bin=bin, dbh=dbhs, elev=x[["elev"]], canht=x[["canht"]]))
    })
    out <- as.data.frame(do.call(rbind, out))
    out$bin <- rep(bins, each=length(dbhs))
    out$elev <- as.factor(x[["elev"]])
    out$canht <- as.factor(x[["canht"]])
    out$dbh <- dbhs
    out
})
dat <- do.call(rbind, dat)

ggplot(dat, aes(dbh, ht, group=canht, color=canht)) + 
  geom_line() +
  geom_line(aes(dbh, lower), alpha=0.3, lty=2) +
  geom_line(aes(dbh, upper), alpha=0.3, lty=2) +
  facet_wrap(~bin + elev, as.table=F) +
  theme_bw()
    
## Nine-panel graph: canopy vs elevation, ht vs dbh
## groups: 
## - canopy: [15,17], [11.5, 13.5], [8, 10]
## - elev: low, mid, high
pp$canbins <- cut(pp$canht, breaks=c(8, 10, 11.5, 13.5, 15, 17))
pp$canbins <- ifelse(pp$canbins %in% levels(pp$canbins)[c(1, 3, 5)], pp$canbins, NA)
mvals <- pp %>% filter(!is.na(canbins)) %>%
  group_by(ELEVCL, canbins) %>%
  summarise(melev=mean(relev), mcanht=mean(canht))

pdat <- pp %>% filter(!is.na(canbins))
  
ldat <- apply(mvals, 1, function(x) {
    out <- t(predInt(bin=1, dbh=dbhs, elev=as.numeric(x[["melev"]]), 
                     canht=as.numeric(x[["mcanht"]])))
    out <- as.data.frame(out)
    out$relev <- as.numeric(x[["melev"]])
    out$canht <- as.numeric(x[["mcanht"]])
    out$dbhs <- dbhs
    out$ELEVCL <- mvals[["ELEVCL"]]
    out$canbins <- mvals[["canbins"]]
    out
})
ldat <- do.call(rbind, ldat)

ggplot(ldat, aes(dbh, ht)) +
  
  
################################################################################
##
##                               Correlations
##
################################################################################
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    ## correlation coefficient
    r <- cor(x, y)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste("r= ", txt, sep = "")
    text(0.5, 0.6, txt)
    
    ## p-value calculation
    p <- cor.test(x, y)$p.value
    txt2 <- format(c(p, 0.123456789), digits = digits)[1]
    txt2 <- paste("p= ", txt2, sep = "")
    if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
    text(0.5, 0.4, txt2)
}

pars <- names(ps)
pairs(AB[, pars], upper.panel = panel.cor)

pars <- c("b", "b1", "b2", "b3")
pairs(AB[, pars], upper.panel = panel.cor)
