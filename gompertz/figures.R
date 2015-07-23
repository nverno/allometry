### figures.R --- 
## Filename: figures.R
## Description: Figures from bootstrap results
## Author: Noah Peart
## Created: Mon Jun  1 14:48:51 2015 (-0400)
## Last-Updated: Thu Jul 23 15:38:40 2015 (-0400)
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
    
################################################################################
##
##             Nine-panel graph: canopy vs elevation, ht vs dbh
##
################################################################################
## groups: 
## - canopy: [15,17], [11.5, 13.5], [8, 10]
## - elev: low, mid, high
pp$canbins <- cut(pp$canht, breaks=c(8, 10, 11.5, 13.5, 15, 17))
pp$canbins <- ifelse(pp$canbins %in% levels(pp$canbins)[c(1, 3, 5)], pp$canbins, NA)
mvals <- pp %>% filter(!is.na(canbins)) %>%
  group_by(ELEVCL, canbins) %>%
  summarise(melev=mean(relev), mcanht=mean(canht))
mvals <- as.data.frame(mvals)

bins <- c(1)
greys <- c("black", "grey40")
colors <- greys[1:length(bins)]
ldat <- lapply(1:nrow(mvals), function(x) {
    out <- lapply(bins, function(bin) {
        t(predInt(bin=bin, dbh=dbhs, elev=as.numeric(mvals[x,"melev"]), 
                     canht=as.numeric(mvals[x,"mcanht"])))
    })
    out <- as.data.frame(do.call(rbind, out))
    out$bin <- as.factor(rep(bins, each=length(dbhs)))
    out$relev <- mvals[x,"melev"]
    out$canht <- mvals[x,"mcanht"]
    out$dbh <- dbhs
    out$ELEVCL <- mvals[x,"ELEVCL"]
    out$canbins <- mvals[x,"canbins"]
    out$color <- as.factor(rep(colors, each=length(dbhs)))
    out
})
ldat <- do.call(rbind, ldat)

## Reorder/name levels
canNames <- c("Low (8-10)", "Mid (11.5-13.5)", "High (15-17)")
elevOrder <- c(2,4,3,1)  # HML
elevNames <- c("", "High (1115-1180)", "Low (808-840)", "Mid (963-1030)")[elevOrder]

ldat$ELEVCL <- factor(ldat$ELEVCL, levels(ldat$ELEVCL)[elevOrder])  # LMH c(3,4,2,1)
ldat$canbins <- factor(ldat$canbins)
levels(ldat$canbins) <- canNames
levels(ldat$ELEVCL) <- elevNames

pdat <- pp %>% filter(!is.na(canbins))
pdat <- as.data.frame(pdat)
pdat$ELEVCL <- factor(pdat$ELEVCL, levels(pdat$ELEVCL)[elevOrder])
pdat$canbins <- factor(pdat$canbins)
levels(pdat$canbins) <- canNames
levels(pdat$ELEVCL) <- elevNames

p <- ggplot(ldat, aes(dbh, ht)) + 
  ## facet_wrap(~ELEVCL + canbins) +
  facet_grid(ELEVCL ~ canbins) +
  geom_point(data=pdat, aes(DBH98, HTTCR98), size=1, alpha=0.6, color="grey10") +
  xlab("Diameter (cm)") + ylab("Height (m)") +
  geom_ribbon(data=ldat[ldat$bin==1,], aes(x=dbh, ymin=lower, ymax=upper), fill="grey", alpha=0.5, lty=0) +
  geom_line(lwd=0.3) + theme_bw() + 
  ## geom_line(aes(dbh, lower), lty=2) +
  ## geom_line(aes(dbh, upper), lty=2) +
  ## scale_color_manual(values = c("black", "grey50"), name="Size\nClasses") +
  ## scale_linetype_manual(values=c(1, 2))
  theme(
      ## Strip labels/grid/background
      panel.grid=element_blank(),
      panel.grid.minor = element_line(),
      ## strip.text=element_blank(), 
      strip.background=element_blank(),
      ## text themes
      axis.text=element_text(size=10), axis.title=element_text(size=12),
      strip.text.x=element_text(size=11),
      strip.text.y=element_text(size=11, angle=270),
      axis.ticks.length=unit(.15, "cm"),
      axis.ticks.margin=unit(0.25, "cm")
  )
p

## Adding outer labels
library(gtable) 
z <- ggplot_gtable(ggplot_build(p))

## add label for right strip
z <- gtable_add_cols(z, unit(1, "cm"))
z <- gtable_add_grob(z, 
  list(rectGrob(gp = gpar(col=NA, fill=NA)), #fill = gray(0.5))),
  textGrob("Elevation (m)", rot = -90, gp = gpar(col = "black", fontsize=13))),
  4, 10, 8, 11, name = paste(runif(2)))

## add label for top strip
z <- gtable_add_rows(z, z$heights[[10]], 2)
z <- gtable_add_grob(z, 
  list(rectGrob(gp = gpar(col = NA, fill=NA)), # fill = gray(0.5))),
  textGrob("Canopy height (m)", gp = gpar(col = "black", fontsize=13))),
  3, 4, 3, 8, name = paste(runif(2)))

## add margins
z <- gtable_add_cols(z, unit(1/8, "line"), 7)
z <- gtable_add_rows(z, unit(1/8, "line"), 3)

## draw it
grid.newpage()
grid.draw(z)

## ggsave("nine_panel.pdf")
################################################################################
##
##                              Appendix figure
##
################################################################################
## Residual elevation on vertical (-100, 0, 100)
## Each panel: Ht vs. DBH for canopy (9, 12.5, 16) for bins (1, 6)
dbhs <- seq(0, max(pp$DBH98), length=100)
elevs <- c(-100, 0, 100)
canhts <- c(9, 12.5, 16)
vars <- expand.grid(elev=elevs, canht=canhts)
bins <- c(1, 6)

## Confidence intervals in ggplot
dat <- apply(vars, 1, function(x) {
    out <- lapply(bins, function(bin) {
        t(predInt(bin=bin, dbh=dbhs, elev=x[["elev"]], canht=x[["canht"]]))
    })
    out <- as.data.frame(do.call(rbind, out))
    out$bin <- as.factor(rep(bins, each=length(dbhs)))
    out$elev <- as.factor(x[["elev"]])
    out$canht <- as.factor(x[["canht"]])
    out$dbh <- dbhs
    out
})
dat <- do.call(rbind, dat)
dat$elev <- factor(dat$elev, levels(dat$elev)[3:1])
    
ggplot(dat, aes(dbh, ht, group=interaction(canht, bin), color=bin, linetype=canht)) + 
  geom_line(lwd=1.05) +
  geom_ribbon(aes(x=dbh, ymin=lower, ymax=upper, fill=bin), alpha=0.2, lty=0) +
  ## geom_line(aes(dbh, lower), alpha=0.3, lty=2) +
  ## geom_line(aes(dbh, upper), alpha=0.3, lty=2) +
  facet_grid(elev ~ .) +
  theme_bw() +
  xlab("DBH (cm)") + ylab("Height (m)") + 
  scale_color_discrete(name="Size\nClasses") +
  scale_fill_discrete(name="Size\nClasses") +
  scale_linetype_discrete(name="Canopy\nHeight") +
  theme(panel.grid=element_blank(),
        axis.text=element_text(size=14), 
        axis.title=element_text(size=14, face="bold"),
        strip.text.y=element_text(size=12, face="bold", angle=270)
        )

## Reorder/name levels
## canNames <- c("Low [8, 10]", "Mid [11.5, 13.5]", "High [15, 17]")
## elevOrder <- c(2,4,3,1)  # HML
## elevNames <- c("", "High [1115, 1180]", "Low [808, 840]", "Mid [963, 1030]")[elevOrder]

ldat$ELEVCL <- factor(ldat$ELEVCL, levels(ldat$ELEVCL)[elevOrder])  # LMH c(3,4,2,1)
ldat$canbins <- factor(ldat$canbins)
levels(ldat$canbins) <- canNames
levels(ldat$ELEVCL) <- elevNames

pdat <- pp %>% filter(!is.na(canbins))
pdat <- as.data.frame(pdat)
pdat$ELEVCL <- factor(pdat$ELEVCL, levels(pdat$ELEVCL)[elevOrder])
pdat$canbins <- factor(pdat$canbins)
levels(pdat$canbins) <- canNames
levels(pdat$ELEVCL) <- elevNames

p <- ggplot(ldat, aes(dbh, ht)) + 
  ## facet_wrap(~ELEVCL + canbins) +
  facet_grid(ELEVCL ~ canbins) +
  geom_point(data=pdat, aes(DBH98, HTTCR98), alpha=0.8, color="grey10") +
  xlab("DBH [cm]") + ylab("Height [m]") +
  geom_ribbon(data=ldat[ldat$bin==1,], aes(x=dbh, ymin=lower, ymax=upper), fill=bin, alpha=0.1, lty=0) +
  geom_line(lwd=1.1) + theme_bw() + 
  ## geom_line(aes(dbh, lower), lty=2) +
  ## geom_line(aes(dbh, upper), lty=2) +
  ## scale_color_manual(values = c("black", "grey50"), name="Size\nClasses") +
  ## scale_linetype_manual(values=c(1, 2))
  theme(
      ## Strip labels/grid/background
      panel.grid=element_blank(), 
      ## strip.text=element_blank(), 
      strip.background=element_blank(),
      ## text themes
      axis.text=element_text(size=14), axis.title=element_text(size=14, face="bold"),
      strip.text.x=element_text(size=12, face="bold"),
      strip.text.y=element_text(size=12, face="bold", angle=270)
  )


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


################################################################################
##
##                              Two-panel graph
##
################################################################################
dbhs <- seq(0, max(pp$DBH98), length=100)
elevs <- c(-150, 0, 150)
canhts <- seq(5, 20, by=5)
cht <- 12.2
## vars <- expand.grid(elev=elevs, canht=canhts)

## ggplot
library(cowplot)
es <- sapply(elevs, function(x) gompertz(ps, dbhs, x, cht))
cs <- sapply(canhts, function(x) gompertz(ps, dbhs, 0, x))

dat <- data.frame(x=rep(dbhs, sum(length(elevs), length(canhts))),
                  y=c(es, cs),
                  group=rep(1:(length(elevs)+length(canhts)), each=100),
                  type=rep(2:1, times=c(length(elevs), length(canhts))*100))

library(grid)
ctext <- list(x=25, y=c(19, 6))
p1 <- ggplot(dat[dat$type == 1, ], aes(x, y, group=group)) +
  geom_line() +
  xlab("Diameter (cm)") +
  ylab("Height (m)") +
  theme_classic() +
  annotate(geom="text", x=ctext$x, y=ctext$y, label=c("C+", "C-")) +
  ylim(0, 20) +
  theme(
      ## Strip labels/grid/background
      panel.grid=element_blank(),
      panel.grid.minor = element_line(),
      ## strip.text=element_blank(), 
      strip.background=element_blank(),
      ## text themes
      axis.text=element_text(size=10), axis.title=element_text(size=12),
      strip.text.x=element_text(size=11),
      strip.text.y=element_text(size=11, angle=270),
      axis.title.x = element_text(vjust=-0.5),
      axis.title.y = element_text(vjust=1.5),
      axis.ticks.length=unit(.15, "cm"),
      axis.ticks.margin=unit(0.25, "cm")
  )

etext <- list(x=15, y=c(9, 11.5))
p2 <- ggplot(dat[dat$type == 2, ], aes(x, y, group=group)) +
  geom_line() +
  xlab("Diameter (cm)") +
  ylab("Height (m)") +
  theme_classic() +
  annotate(geom="text", x=etext$x, y=etext$y, label=c("E+", "E-")) +
  ylim(0, 20) +
  theme(
      ## Strip labels/grid/background
      panel.grid=element_blank(),
      panel.grid.minor = element_line(),
      ## strip.text=element_blank(), 
      strip.background=element_blank(),
      ## text themes
      axis.text=element_text(size=10), axis.title=element_text(size=12),
      strip.text.x=element_text(size=11),
      strip.text.y=element_text(size=11, angle=270),
      axis.title.x = element_text(vjust=-0.4),
      axis.title.y = element_text(vjust=1.4),
      axis.ticks.length=unit(.15, "cm"),
      axis.ticks.margin=unit(0.25, "cm")
  )

## Construct layout
grid.newpage()
pushViewport(viewport(layout=grid.layout(1, 2)))
print(p1, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(p2, vp=viewport(layout.pos.row=1, layout.pos.col=2))

## pdf/view function
f <- function(width=5, height=5, fname="a", temp=T) {
    if (temp) {
        pdf(tf <- tempfile(fileext = ".pdf"), height = height, width=width)
    } else
        pdf(fname, height=height, width=width)
    plot_grid(p1, p2, labels=c("a)", "b)"), ncol=2)
    dev.off() 
    shell.exec(tf) 
}
f()

## Arrange graphs
library(cowplot)
pdf("two_panel.pdf", width=8, height=4)
plot_grid(p1, p2, labels=c("a)", "b)"), ncol=2)
dev.off()

## Base R
par(mfrow = c(1, 2))
plot(pp$DBH98, pp$HTTCR98, type="n", frame.plot=F)
axis(4)

for (canht in canhts) {
    ys <- gompertz(ps, dbhs, elev=0, canht=canht)
    points(dbhs, ys, type="l")
}

plot(pp$DBH98, pp$HTTCR98, type="n")
for (elev in elevs) {
    ys <- gompertz(ps, dbhs, elev=elev, canht=12.2)
    points(dbhs, ys, type="l")
}
