## Compare plots in mid east that vary drastically in changes in ht / canht,
## some are top performers others bottom performers in this category
source("~/work/functions/functions-datatrans.R")
library(ggplot2)
library(grid)
library(plyr)

canhts <- read.csv("~/work/data/data/boles/canhts.csv")
pp <- read.csv("~/work/data/data/dynamicallometry/moose-long-canopies.csv")

## add survival indicator column, if tree died at any time during census
##  it gets a 1
pp$died <- ifelse(!is.na(pp$yrmort), 1, 0)

## change pp time scale, pplot to factor, remove dead trees in 86/87
pp[pp$time == 10,]$time <- 110
pp$pplot <- as.factor(pp$pplot)
pp <- pp[!pp$yrmort %in% c(1986, 1987),]

## paths colored by survival
tst <- pp[is.na(pp$eht) & pp$spec %in% c("ABBA", "PIRU") & pp$elevcl == "M" & pp$aspcl == "E",]
tst <- droplevels(tst[!tst$pplot %in% c(1,2,3),])

ggplot(tst, aes(dbh, ht/canht, group = id, col = died)) +
    geom_path(arrow = arrow(), alpha = 0.5) + geom_point(alpha = 0.5) +
    facet_wrap(~pplot, ncol = 2) + ggtitle("multiple species") + geom_hline(yintercept=1, lty = 2)
    #xlim(0,5)


## Look specifically at 8, 9, 11
## Growth relative to canopy height
tst <- droplevels(tst[tst$pplot %in% c(8,9,11),])
tst[tst$time == 87,]$time <- 86
tst <- subset(tst, !is.na(canht))

ggplot(tst, aes(dbh, ht/canht, group = id, col = died)) +
    geom_path(arrow = arrow(), alpha = 0.5) + geom_point(alpha = 0.5) +
    facet_wrap(~pplot, ncol = 3) + ggtitle("Mid East Performers") +
    geom_hline(yintercept=1, lty = 2)
    #xlim(0,5)

## Absolute growth
ggplot(tst, aes(dbh, ht, group = id, col = died)) +
    geom_path(arrow = arrow(), alpha = 0.5) + geom_point(alpha = 0.5) +
    facet_wrap(~pplot, ncol = 3) + ggtitle("Mid East Performers") +
    geom_hline(yintercept=1, lty = 2)
    #xlim(0,5)

## ht/dbh
ggplot(tst, aes(dbh, ht/dbh, group = id, col = died)) +
    geom_path(arrow = arrow(), alpha = 0.5) + geom_point(alpha = 0.5) +
    facet_wrap(~pplot, ncol = 3) + ggtitle("Mid East Performers")

## canht
ggplot(tst, aes(time, canht, group = pplot, col = pplot)) +
    geom_point() + geom_path(arrow = arrow()) + facet_wrap(~pplot)


## density changes
dens <- ddply(tst, .(pplot, time), .fun = function(x) {
    x <- droplevels(x);
    data.frame(dens = nrow(x[!is.na(x$dbh) | !is.na(x$ht),]))
})

ggplot(dens, aes(time, dens, group = pplot, col = pplot)) + geom_point() +
    geom_path(arrow = arrow())

