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
abba_mats<- make.neighbor.matrices(abbas, sr, ind.var=ind.var, bigger=TRUE)

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
piru_mats<- make.neighbor.matrices(pirus, sr, ind.var=ind.var, bigger=TRUE)


x <- 2
y <- 4
tst <- function(x, bool1=TRUE, y=y, bool2=FALSE) {
    ifelse(bool2, print("bool2"), print("notbool2") )
    ifelse(bool1, print("bool1"), print("notbool1") )
    return (x * y)
}
