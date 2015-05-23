## Separate out the trees that had extreme changes in allometry in either negative
##  or positive slope
## compare change in slope along s-graph by survival
source("~/work/functions/functions-datatrans.R")
library(ggplot2)
library(grid)

canhts <- read.csv("~/work/data/data/boles/canhts.csv")
pp <- read.csv("~/work/data/data/dynamicallometry/moose-long-canopies.csv")

## add survival indicator column, if tree died at any time during census
##  it gets a 1
pp$died <- ifelse(!is.na(pp$yrmort), 1, 0)

## transform to wide
## time-varying columns
ns <- c("stat", "decm", "dbh", "ht", "ba", "bv", "eht", "cht", "cpos", "dbhgrowth",
        "htgrowth", "bagrowth", "bvgrowth", "priordbh", "priorht", "priorba",
        "priorbv", "canht")

## transform to wide
wide <- reshape(pp, v.names = ns, direction = "wide")

## Find extreme indviduals (across entires sampling interval)
## those with large/small gains in ht/canht
wide$p1 <-
    ifelse(wide$pplot < 16 & wide$pplot > 3,
        (wide$ht.10 - wide$canht.10) - (wide$ht.86 - wide$canht.86),
           ifelse(wide$pplot >= 16,
        (wide$ht.10 - wide$canht.10) - (wide$ht.87 - wide$canht.87), NA))

qs <- quantile(wide$p1, na.rm=T)
names(quantile(wide$p1, na.rm=T))
## look at top performers
top <- subset(wide, p1 > qs[["75%"]])
table(top$spec)
table(top$pplot)

## bottom performers
bottom <- subset(wide, p1 < qs[["25%"]])
table(bottom$spec) ## BECO overrepresented
table(bottom$pplot)

## top perfomer plots: 9, 11, 18, 24, 27
p11 <- subset(wide, pplot == 11)
unique(p11$canht.86)
unique(p11$canht.10)

## bottom plots: 8, 13, 14, 20, 25


## What is different between 8, 9, 11? (all ME)
## 25, 27? (HW)

## Absolute growth
wide$totgrowth <-
    ifelse(wide$pplot < 16 & wide$pplot > 3,
        (wide$ht.10 - wide$ht.86),
           ifelse(wide$pplot >= 16,
        (wide$ht.10 -  wide$ht.87), NA))

qs2 <- quantile(wide$totgrowth, na.rm=T)
## look at top performers
toptot <- subset(wide, totgrowth > qs2[["75%"]])
table(toptot$spec)
table(toptot$pplot)

## bottom performers
bottomtot <- subset(wide, totgrowth < qs2[["25%"]])
table(bottomtot$spec) ## BECO overrepresented
table(bottomtot$pplot)

## plot 16
p16 <- subset(wide, pplot ==16)
quantile(p16$totgrowth, na.rm=T)
