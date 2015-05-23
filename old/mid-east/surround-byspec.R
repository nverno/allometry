## Examine the effect of crowdedness on bagrowth/htgrowth by species in the mid-east plots
source("~/work/functions/functions-neighborhood.R")

library(ggplot2)
library(grid)
library(plyr)

pp <- read.csv("~/work/data/data/dynamicallometry/moose-long-canopies.csv")
## mid-east
dat <- subset(pp, pplot %in% c(8,9,11))

##################################################################################
## globals
sr <- 2
dep.var <- "bagrowth"
ind.var <- "ba"
# define targets and neighbors, for moose data
targets <- subset(dat, bqudx < (12-sr) & bqudx > (-1 + sr) & bqudy < (12 - sr) &
                  bqudy > (-1 + sr) & stat=="ALIVE")
neighbors <- subset(dat, bqudx < 11 & bqudx > 0 & bqudy < 11 &
                    bqudy > 0 & stat=="ALIVE")
specs <- c("ABBA","PIRU","BECO")


##################################################################################
##
##    ABBA
##
##################################################################################
spec <- "ABBA"

# remove trees that dont satisfy certain conditions
grew <- which(!is.na(targets[,dep.var]) & targets$spec==spec & targets[,dep.var]>0)
abbas <- targets[grew,]

# make neighbor matrices using square radius (i.e bqudx,bqudy)
abba_mats<- mnm(abbas, neighbors, sr, ind.var=ind.var)

## Assign Neighbor variables
abbas$sumBa <- rowSums(abba_mats[["variable"]], na.rm=T)
abbas$nDen <- abba_mats$number_neighbors

## visualize
table(abbas$pplot)
table(abbas$pplot, abbas$time)

## Growth vs. summed neighbor BA
ggplot(abbas, aes(sumBa, bagrowth, group = id, col = factor(pplot))) + geom_path(arrow = arrow())

## Growth vs. neighbor density
ggplot(abbas, aes(nDen, bagrowth, group = id, col = factor(pplot))) +
    geom_path(arrow = arrow()) + xlim(0, 20)

## Neighborhood density by size of tree
ggplot(abbas, aes(ba, nDen, col = pplot, group = factor(pplot))) + geom_point() +
    geom_smooth(se = F) + facet_wrap(~time)

## ht/dbh vs nDen
library(ggplot2)
ggplot(abbas, aes(nDen, ht/dbh)) +
    geom_path(arrow = arrow(), alpha = 0.5, aes(group = id)) +
    geom_point(aes(size = ba)) +
    facet_wrap(~pplot, ncol = 3) + ggtitle("Mid East Performers") +
    xlim(0,20) + ylim(0.3,1.5)

ggplot(abbas, aes(nDen, ht/dbh)) +
    geom_point(alpha = 0.5) + facet_wrap(~pplot) +
    ggtitle("Mid East Performers") +
    xlim(0,20) + ylim(0.3,1.5) + geom_smooth()

## Surroundedness
abbas$crowd <- percSurround(sr, abbas, abba_mats)

## Visualize bagrowth vs crowdedness
ggplot(abbas, aes(crowd, bagrowth)) + geom_point()
ggplot(abbas, aes(crowd, bagrowth, group = id, col = factor(pplot))) + geom_path(arrow = arrow())

## bagrowth vs crowdedness * sum neighbor BA
ggplot(abbas, aes(sumBa*crowd, htgrowth/dbhgrowth, group = id, col = factor(pplot))) +
    geom_path(arrow = arrow())

## simple glm
mod1 <- glm(bagrowth ~ priorba * (crowd + sumBa), data = abbas)
mod2 <- glm(bagrowth ~ priorba * crowd, data = abbas)
mod3 <- glm(bagrowth ~ priorba * sumBa, data = abbas)
anova(mod1, mod2, mod3, test = "Chisq")
plot(mod2)
## plot
tst <- abbas
tst$pred <- predict(mod1)

ggplot(tst, aes(priorba, bagrowth, color = crowd)) + geom_point() +
    geom_point(aes(priorba, pred), pch = 3)

##################################################################################
##
##    PIRU
##
##################################################################################
spec <- "PIRU"

# remove trees that dont satisfy certain conditions
grew <- which(!is.na(targets[,dep.var]) & targets$spec==spec & targets[,dep.var]>0)
pirus <- targets[grew,]

# make neighbor matrices using square radius (i.e bqudx,bqudy)
piru_mats<- make.neighbor.matrices(pirus, neighbors, sr, ind.var=ind.var)


##################################################################################
## Timing
## library(rbenchmark)
## benchmark(make.neighbor.matrices(targets, neighbors, sr, ind.var=ind.var, bigger=TRUE),
##           mnm(targets, neighbors, sr, ind.var=ind.var, bigger=TRUE),
##           columns = c("test", "replications", "elapsed", "relative"),
##           order = "relative", replications = 5)

## 2                   mnm(targets, neighbors, sr, ind.var = ind.var, bigger = TRUE)
## 1 make.neighbor.matrices(targets, neighbors, sr, ind.var = ind.var, bigger = TRUE)
##   replications elapsed relative
## 2            5  16.862    1.000
## 1            5  32.681    1.938
