### setup.R --- 
## Filename: setup.R
## Description: Setup data for bootstrap.Rmd
## Author: Noah Peart
## Created: Wed May 27 12:59:58 2015 (-0400)
## Last-Updated: Mon Jun  1 10:15:32 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/allometry/gompertz/model.R")
source("~/work/allometry/gompertz/run_boot.R")
## source("~/work/allometry/gompertz/zach_setup.R")
pp <- readRDS("~/work/allometry/gompertz/temp/pp.rds")
res <- lm(ELEV ~ cht98, data=pp)
pp$relev <- residuals(res)

## Bootstrap for 98 data, sampling equally from quantiles of DBH98
## yr <- 98
## stat <- paste0("STAT", yr)
## dbh <- paste0("DBH", yr)
## ht <- paste0("HTTCR", yr)
## canht <- paste0("cht", yr)
## elev <- "relev"
## dat <- pp[pp[,stat] == "ALIVE" & pp$SPEC == "ABBA" & !is.na(pp[,dbh]) &
##              !is.na(pp[,ht]), ]
## ps <- readRDS("~/work\\ecodatascripts\\vars\\heights\\gompertz\\full\\abba\\abba_98.rds")

## Parameters from manuscript
paper_ps <- list(a=0.199, a1=0.000251, a2=0.00511, a3=-0.0000141, b=2.31, b1=0.00410, b2=0.92, b3=-0.000271,
           sd=1)
ps <- readRDS("~/work/allometry/gompertz/temp/ps.rds")

