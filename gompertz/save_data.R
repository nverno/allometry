### save_data.R --- 
## Filename: save_data.R
## Description: Save final data
## Author: Noah Peart
## Created: Thu Jul 23 12:11:01 2015 (-0400)
## Last-Updated: Thu Jul 23 16:14:00 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/allometry/gompertz/setup.R", chdir=T)
library(ggplot2)

## Data cleaning
## PPLOT 7, TAG 98641
pp <- pp[!(pp$PPLOT==7 & pp$TAG==98641), ]

## Height, canopy, elevation, DBH
dat <- pp[, c("HTTCR98", "DBH98", "ELEV", "cht98")]
names(dat) <- c("HEIGHT", "DIAMETER", "ELEVATION",  "CANOPYHEIGHT")
write.csv(dat, "data.csv")

################################################################################
##
##                             A couple figures
##
################################################################################
## HT vs DBH
ggplot(pp, aes(DBH98, HTTCR98)) +
  geom_point(alpha=0.7, size=1.2) +
  theme_classic() +
  xlab("Diameter [cm]") +
  ylab("Height [m]") +
  theme(axis.title.x = element_text(vjust=-0.5),
        axis.title.y = element_text(vjust=1.5))

## Canopy vs Elev
ggplot(pp, aes(ELEV, cht98)) +
  geom_point(alpha=0.7, size=1.2) +
  theme_classic() +
  xlab("Elevation [m]") +
  ylab("Canopy height [m]") +
  theme(axis.title.x = element_text(vjust=-0.5),
        axis.title.y = element_text(vjust=1.5))


