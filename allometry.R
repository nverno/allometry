### Master Script for Allometry Project
# Project goals:
# 1. species comparisons by static and dynamic allometry
# 2. Neighborhood effects on allometry

source("functions.R")
source("neighborhood-models.R")

## Preparing the data
## source("read-data.R")
## source("clean-data.R")
## source("explore-df-data.R")
## source("make-neighbor-values.R")
## source("make-multiple-neighborhoods.R")
## source("make-long-sr2.R")
## source("make-long.R")
## source("make-long-nohood.R") #**current**
## source("make-long-multiple.R")
## source("make-sizeclasses.R")
## source("make-sclass-byplot.R")
## source("make-sclass-byelev.R")

## Second-order polynomials
## source("significant-2nd-order.R")
## source("neighbor-significance.R")
## source("most-sig-neighbor.R")
## source("compare-fits-to-nclasses-byplot.R")
## source("compare-fits-to-nclasses-byelev.R")

## Neighbor model using only target size and max height within plot
## source("model-maxht.R") # with DF data

## LRS and rGR
## source("scott-model.R")
## source("rGR-envelope-SIandSDP.R")
## source("graph-rgr-sisdp.R")

## Stand Development
## source("test-df-data.R") # check long data against original
## source("make-long-dfonlyderived.R")
## source("explore-self-thinning.R")
## source("fit-self-thinning.R")
## source("compute-sdps.R")

## Grouping by Stand Development and SI
## source("stand-index.R")

## Envelope fitting for maximum growth potential
## source("size-envelope.R")
## source("size-envelope-byplot.R")
## source("compare-envelope-models.R")
## source("size-envelope-sisdp.R")

## More complex neighborhood modelling
## source("make-neighbor-matrices.R") #**current**
## source("neighborhood-run-MLE.R") #**current**
## source("compare-nmodels.R") #**current**
## source("automate-model-comparison.R") #**current**

## MLE model fitting platform
## source("run-MLE-model.R")

## Visualizing the data
## source("exploratory-graphics.R")
## source("graph-models.R")
## source("visualize-MLE-fits.R")
## source("visualize-rgr-si-sdp.R")
