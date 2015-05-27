### zach_setup.R --- 
## Filename: zach_setup.R
## Description: 
## Author: Noah Peart
## Created: Sat May 23 12:59:35 2015 (-0400)
## Last-Updated: Wed May 27 14:12:13 2015 (-0400)
##           By: Noah Peart
######################################################################
pp <- read.csv("~/work/treedata/pp.csv")

## local canopies -- follow zach's subsetting procedure and local canopy construction
pp <- pp[!is.na(pp$HTTCR98) & pp$HTTCR98 > 0, ]
pp$cht98 <- NA
for (i in 1:nrow(pp))
    pp$cht98[i] <- max(pp[pp$BQUDX >= pp$BQUDX[i]-1 & pp$BQUDY >= pp$BQUDY[i]-1 &
                              pp$BQUDX <= pp$BQUDX[i]+1 & pp$BQUDY <= pp$BQUDY[i]+1 & 
                                  pp$PPLOT == pp$PPLOT[i],
                          "HTTCR98"])

## w/o target included
pp$cht_notarget98 <- NA
for (i in seq_along(cht_notarget)) {
    hood <- pp[pp$BQUDX >= pp$BQUDX[i]-1 & 
                   pp$BQUDY >= pp$BQUDY[i]-1 &
                       pp$BQUDX <= pp$BQUDX[i]+1 & 
                           pp$BQUDY <= pp$BQUDY[i]+1 & 
                               pp$PPLOT == pp$PPLOT[i] &
                                   pp$TAG != pp$TAG[i], ]
    pp$cht_notarget98[i] <- ifelse (nrow(hood)>0, max(hood[,"HTTCR98"]), 0)
}

pp <- pp[!is.na(pp$DBH98) & pp$DBH98 > 0 & !is.na(pp$cht98) & pp$BQUDX > 1 & pp$BQUDX < 10 &
             pp$BQUDY > 1 & pp$BQUDY < 10, ]
pp <- pp[pp$SPEC == "ABBA", ]

## Parameters from manuscript
ps <- list(a=0.199, a1=0.000251, a2=0.00511, a3=-0.0000141, b=2.31, b1=0.00410, b2=0.92, b3=-0.000271,
           sd=1)

saveRDS(pp, "~/work/allometry/gompertz/temp/pp.rds")
