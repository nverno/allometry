library(plyr)
library(ggplot2)
library(grid)

canhts <- read.csv("~/work/data/data/boles/canhts.csv")
pp <- read.csv("~/work/data/data/dynamicallometry/moose-long-canopies.csv")

## add survival indicator column, if tree died at any time during census
##  it gets a 1
pp$died <- ifelse(!is.na(pp$yrmort), 1, 0)

## no estimated heights
pp <- subset(pp, is.na(eht) & !yrmort %in% c(1986, 1987))
ggplot(pp, aes(dbh, ht/canht, color = died, group = died)) + geom_point() +
    geom_smooth()

## curves across all data very similar for those that died and survived

## by plot, aspect, elevation
## ABBAs
abba <- subset(pp, spec == "ABBA")
ggplot(abba, aes(dbh, ht/canht, color = died, group = died)) + geom_point(alpha = 0.5) +
    geom_smooth() + facet_wrap(~aspcl)

ggplot(abba, aes(dbh, ht/canht, color = died, group = died)) + geom_point(alpha = 0.5) +
    geom_smooth() + facet_wrap(~elevcl + time)

## PIRU
piru <- subset(pp, spec == "PIRU")
ggplot(piru, aes(dbh, ht/canht, color = died, group = died)) + geom_point(alpha = 0.5) +
    geom_smooth() + facet_wrap(~aspcl)

ggplot(piru, aes(dbh, ht/canht, color = died, group = died)) + geom_point(alpha = 0.5) +
    geom_smooth() + facet_wrap(~elevcl + time)


##############################################################################
## Separate trees that were higher in the canopy than expected given their DBH
## Use regular power fit?

## By PLOT
abba <- subset(pp, spec == "ABBA" & pplot > 3 & !is.na(canht) & !is.na(ht) & !is.na(dbh))
above <- ddply(abba, .(pplot), .fun = function(x) {
    x <- droplevels(x);
    mod <- nls( (ht/canht) ~ a * dbh ^ b, start = list(a = 0.5, b = 1), data = x )
    data.frame(id = x$id, pplot = x$pplot, time = x$time,
               above = x$ht/x$canht > predict(mod, newdata = x, na.action = NA))
})

## join above data back to abba
abba <- merge(abba, above, by = c("pplot", "time", "id"))

ggplot(abba, aes(dbh, ht/canht, color = died, group = died)) + geom_point(alpha = 0.5) + facet_wrap(~above) +
    geom_smooth(method = "loess")


## By ASPCL
abba <- subset(pp,spec == "ABBA" & pplot > 3 & !is.na(canht) & !is.na(ht) & !is.na(dbh))
above <- ddply(abba, .(aspcl), .fun = function(x) {
    x <- droplevels(x);
    mod <- nls( (ht/canht) ~ a * dbh ^ b, start = list(a = 0.5, b = 1), data = x )
    data.frame(id = x$id, pplot = x$pplot, time = x$time,
               above = x$ht/x$canht > predict(mod, newdata = x, na.action = NA))
})
## join above data back to abba
abba <- merge(abba, above, by = c("pplot", "time", "id", "aspcl"))
ggplot(abba, aes(dbh, ht/canht, color = died, group = died)) + geom_point(alpha = 0.5) +
    facet_wrap(~above + aspcl) +
    geom_smooth(method = "loess")

## By ELEVCL
abba <- subset(pp,spec == "ABBA" & pplot > 3 & !is.na(canht) & !is.na(ht) & !is.na(dbh))
above <- ddply(abba, .(elevcl), .fun = function(x) {
    x <- droplevels(x);
    mod <- nls( (ht/canht) ~ a * dbh ^ b, start = list(a = 0.5, b = 1), data = x )
    data.frame(id = x$id, pplot = x$pplot, time = x$time,
               above = x$ht/x$canht > predict(mod, newdata = x, na.action = NA))
})
## join above data back to abba
abba <- merge(abba, above, by = c("pplot", "time", "id", "aspcl"))
ggplot(abba, aes(dbh, ht/canht, color = died, group = died)) + geom_point(alpha = 0.5) +
    facet_wrap(~above + aspcl) +
    geom_smooth(method = "loess")
