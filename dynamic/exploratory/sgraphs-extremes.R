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
