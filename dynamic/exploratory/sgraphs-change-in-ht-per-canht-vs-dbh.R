## Change in ht/canht vs. dbh
source("~/work/functions/functions-datatrans.R")
library(ggplot2)
library(grid)

canhts <- read.csv("~/work/data/data/boles/canhts.csv")
pp <- read.csv("~/work/data/data/dynamicallometry/moose-long-canopies.csv")
ppwide <- read.csv("~/work/data/data/moose-wide.csv")

## add survival indicator column, if tree died at any time during census
##  it gets a 1
pp$died <- ifelse(!is.na(pp$yrmort), 1, 0)

## time-varying columns
ns <- c("stat", "decm", "dbh", "ht", "ba", "bv", "eht", "cht", "cpos", "dbhgrowth",
        "htgrowth", "bagrowth", "bvgrowth", "priordbh", "priorht", "priorba",
        "priorbv", "canht")

## transform to wide
wide <- reshape(pp, v.names = ns, direction = "wide")


## Function to check if NA during previous sampling period, given vector of sampling times
##  column name, and vector of special times signifying that these times should be treated
##  as being the same sampling period
## Returns: NA if NA, otherwise the column name of the previous period
prevNA <- function(dat, row, col, times = c(86, 87, 98, 10), joined = list(c(86, 87))) {
    current <- as.numeric(gsub("[^0-9]", "", col))
    periods <- lapply(joined, FUN = function(x) { match(x, times) })
    this.period <- which( sapply(periods, FUN = function(x) { match(current, times) %in% x }) )
    while (this.period > 0) {

    }
    times[current] %in% periods
    ##    past <-
}

## make change in ht/canht column, `dhtcan`
col <- "dbh.98"
tst <- gsub("[^0-9]", "", col)
## wide$dhtcan.98 <- if (!is.na(dhtcan.98))

times = c(86, 87, 98, 10)
joined = list(c(86, 87), c(98, 10))

periods <- lapply(joined, FUN = function(x) {
    match(x, times)
})
