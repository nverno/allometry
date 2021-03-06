## Create a neighborhood model with crowding and dbh as independent variables
##  and growth in height or dbh as dependent variables
source("~/work/functions/functions-neighborhood.R")
source("~/work/functions/functions-datatrans.R")
source("~/work/functions/functions-growth.R")
source("~/work/functions/functions.R")

library(ggplot2)
library(grid)
library(plyr)

canhts <- read.csv("~/work/data/data/boles/canhts.csv")
pp <- read.csv("~/work/data/data/dynamicallometry/moose-long-canopies.csv")

## add survival indicator column, if tree died at any time during census
##  it gets a 1
pp$died <- ifelse(!is.na(pp$yrmort), 1, 0)

## choose sr(sr must be >= 1) and other variables
sr <- 2
spec <- "ABBA"
dep.var <- "bagrowth"
ind.var <- "ba"

## mid-east subset
dat <- subset(pp, pplot %in% c(8,9,11))

## define targets and neighbors, for moose data
targets <- subset(dat, bqudx < (12-sr) & bqudx > (-1 + sr) & bqudy < (12 - sr) &
                  bqudy > (-1 + sr) & stat=="ALIVE")
neighbors <- subset(dat, bqudx < 11 & bqudx > 0 & bqudy < 11 &
                    bqudy > 0 & stat=="ALIVE")

## mod <- nls(ht ~ a*dbh^b, start = list(a=1,b=1), data = targets)
## xs <- 1:100
## plot(targets$dbh, predict(mod, targets))
## points(targets$ht, (targets$ht/1.67)^(1/0.67), col="red")
x = 1:100
y = 2*(x + rnorm(100))
l1 <- lm(x ~ y)
l2 <- lm(y ~ x)
ll1 <- lm(dbh ~ ht, data = targets)
ll2 <- lm(ht ~ dbh, data = targets)

## remove trees that dont satisfy certain conditions
grew <- which(!is.na(targets[,dep.var]) & targets$spec==spec & targets[,dep.var]>0)
targets <- targets[grew,]

## make neighbor matrices using square radius (i.e bqudx,bqudy)
mats <- make.neighbor.matrices(targets, neighbors, sr, ind.var=ind.var, bigger=TRUE)

## Assign Neighbor variables
targets$sumBa <- rowSums(mats[["variable"]], na.rm=T)
targets$nDen <- rowSums(!is.na(mats[["variable"]]))

## visualize
table(targets$pplot)
table(targets$pplot, targets$time)

## Growth vs. summed neighbor BA
ggplot(targets, aes(sumBa, bagrowth, group = id, col = factor(pplot))) + geom_path(arrow = arrow())

## Growth vs. neighbor density
ggplot(targets, aes(nDen, bagrowth, group = id, col = factor(pplot))) +
    geom_path(arrow = arrow()) + xlim(0, 20)

## Neighborhood density by size of tree
ggplot(targets, aes(ba, nDen, col = pplot, group = factor(pplot))) + geom_point() +
    geom_smooth(se = F) + facet_wrap(~time)

## ht/dbh vs nDen
library(ggplot2)
ggplot(targets, aes(nDen, ht/dbh, col = died)) +
    geom_path(arrow = arrow(), alpha = 0.5, aes(group = id)) +
    geom_point(aes(size = ba)) +
    facet_wrap(~pplot, ncol = 3) + ggtitle("Mid East Performers") +
    xlim(0,20) + ylim(0.3,1.5)

ggplot(targets, aes(nDen, ht/dbh, col = died)) +
    geom_point(alpha = 0.5) + facet_wrap(~pplot) +
    ggtitle("Mid East Performers") +
    xlim(0,20) + ylim(0.3,1.5) + geom_smooth()

## Surroundedness
targets$crowd <- percSurround(sr, targets, mats)

## Visualize bagrowth vs crowdedness
ggplot(targets, aes(crowd, bagrowth)) + geom_point()
ggplot(targets, aes(crowd, bagrowth, group = id, col = factor(pplot))) + geom_path(arrow = arrow())

## bagrowth vs crowdedness * sum neighbor BA
ggplot(targets, aes(sumBa*crowd, htgrowth/dbhgrowth, group = id, col = factor(pplot))) +
    geom_path(arrow = arrow())

## simple glm
mod1 <- glm(bagrowth ~ priorba * (crowd + sumBa), data = targets)
mod2 <- glm(bagrowth ~ priorba * crowd, data = targets)
mod3 <- glm(bagrowth ~ priorba * sumBa, data = targets)
anova(mod1, mod2, mod3, test = "Chisq")
plot(mod2)
## plot
tst <- targets
tst$pred <- predict(mod2)

ggplot(tst, aes(priorba, bagrowth, color = crowd)) + geom_point() +
    geom_point(aes(priorba, pred), pch = 3)
