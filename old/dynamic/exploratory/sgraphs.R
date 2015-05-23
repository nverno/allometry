## create S-graphs for Moosilauke species.
## height/canopy height vs dbh/ht
library(ggplot2)
library(grid)

canhts <- read.csv("~/work/data/data/boles/canhts.csv")
pp <- read.csv("~/work/data/data/dynamicallometry/moose-long-canopies.csv")

## ABBAs
abbas1 <- pp[pp$spec == "ABBA" & is.na(pp$eht),]
abbas <- pp[pp$spec == "ABBA",]

## plot 4
getOption("device")()
p4 <- subset(abbas1, pplot == 4)
ggplot(p4, aes(dbh/ht, ht/canht, group = id)) +
    geom_path(arrow = arrow()) + xlim(0,5)

getOption("device")()
p4 <- subset(abbas, pplot == 4)
ggplot(p4, aes(dbh/ht, ht/canht, group = id)) +
    geom_path(arrow = arrow()) + xlim(0,5)


## plot 8
p8 <- subset(abbas, pplot == 8)
ggplot(p8, aes(dbh/ht, ht/canht, group = id)) +
    geom_path(arrow = arrow()) + xlim(0,5)

## PIRUs
pirus <- pp[pp$spec == "PIRU",]
pirus1 <- pp[pp$spec == "PIRU" & is.na(pp$eht),]

## plot 4
p4 <- subset(pirus, pplot == 4)
ggplot(p4, aes(dbh/ht, ht/canht, group = id)) +
    geom_point(col = "red") +
    geom_path(arrow = arrow()) + xlim(0,5)

getOption("device")()
p4 <- subset(pirus1, pplot == 4)
ggplot(p4, aes(dbh/ht, ht/canht, group = id)) +
    geom_point(col = "red") +
    geom_path(arrow = arrow()) + xlim(0,5)

## plot 8
p8 <- subset(pirus, pplot == 8)
ggplot(p8, aes(dbh/ht, ht/canht, group = id)) +
    geom_path(arrow = arrow()) + xlim(0,5)

## plot 12
p12 <- subset(pirus, pplot == 12)
ggplot(p12, aes(dbh/ht, ht/canht, group = id)) +
    geom_path(arrow = arrow()) + xlim(0,5)

## mid east
p6 <- subset(pirus, pplot %in% c(7:12))
ggplot(p6, aes(dbh/ht, ht/canht, group = id)) +
    geom_point(col = "red") +
    geom_path(arrow = arrow()) + xlim(0,4)

## BECOs
becos <- pp[pp$spec == "BECO",]

## plot 4
p4 <- subset(becos, pplot == 4)
ggplot(p4, aes(dbh/ht, ht/canht, group = id)) +
    geom_path(arrow = arrow()) + xlim(0,5)

## plot 8
p8 <- subset(becos, pplot == 8)
ggplot(p8, aes(dbh/ht, ht/canht, group = id)) +
    geom_path(arrow = arrow()) + xlim(0,5)

## high east
p6 <- subset(becos, pplot %in% c(13, 14, 15))
ggplot(p6, aes(dbh/ht, ht/canht, group = id)) +
    geom_path(arrow = arrow()) + xlim(0,5)

## mid east
p6 <- subset(becos, pplot %in% c(7:12))
ggplot(p6, aes(dbh/ht, ht/canht, group = id)) +
    geom_path(arrow = arrow()) + xlim(0,5)

## multiple species, pplot 4
tst <- pp[is.na(pp$eht) & pp$spec %in% c("ABBA", "PIRU", "BECO") & pp$pplot == 4,]
ggplot(tst, aes(dbh/ht, ht/canht, group = id)) +
    geom_path(arrow = arrow()) + xlim(0,4) + geom_point(col = "red") +
    facet_wrap(~spec) + opts(title = "Plot 4")

## multiple species, pplot 24
tst <- pp[is.na(pp$eht) & pp$spec %in% c("ABBA", "PIRU", "BECO") & pp$pplot == 24,]
ggplot(tst, aes(dbh/ht, ht/canht, group = id)) +
    geom_path(arrow = arrow()) + xlim(0,4) + geom_point(col = "red") +
    facet_wrap(~spec) + opts(title = "Plot 24")

## multiple species, pplot 27
tst <- pp[is.na(pp$eht) & pp$spec %in% c("ABBA", "PIRU", "BECO") & pp$pplot == 27,]
ggplot(tst, aes(dbh/ht, ht/canht, group = id)) +
    geom_path(arrow = arrow()) + xlim(0,4) + geom_point(col = "red") +
    facet_wrap(~spec) + opts(title = "Plot 27")

## multiple species
tst <- pp[is.na(pp$eht) & pp$spec %in% c("ABBA", "PIRU") & pp$elevcl == "M" ,]#& pp$aspcl == "E",]
ggplot(tst, aes(dbh, ht/canht, group = id, col = time)) +
    geom_path(arrow = arrow(), alpha = 0.5) + geom_point(alpha = 0.5) +
    facet_wrap(~spec + aspcl, ncol = 2) + ggtitle("multiple species") + geom_hline(yintercept=1, lty = 2)
    #xlim(0,5)
