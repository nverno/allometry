### save_data.R --- 
## Filename: save_data.R
## Description: Save final data
## Author: Noah Peart
## Created: Thu Jul 23 12:11:01 2015 (-0400)
## Last-Updated: Tue Aug 18 12:58:32 2015 (-0400)
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
p1 <- ggplot(pp, aes(DBH98, HTTCR98)) +
  geom_point(alpha=0.7, size=0.5) +
  theme_classic() +
  xlab("Diameter [cm]") +
  ylab("Height [m]") +
  theme(
      ## Strip labels/grid/background
      panel.grid=element_blank(),
      panel.grid.minor = element_line(),
      strip.background=element_blank(),
      ## text themes
      axis.text=element_text(size=9, face="plain"),
      axis.title=element_text(size=9, face="plain"),
      axis.title.x = element_text(vjust=-0.4),
      axis.title.y = element_text(vjust=1.4),
      axis.ticks.length=unit(.15, "cm"),
      axis.ticks.margin=unit(0.25, "cm")
  )

pdf("ht_dbh.pdf", width=2.75591, height=2.75591)
plot(p1)
dev.off()

## Canopy vs Elev
p2 <- ggplot(pp, aes(ELEV, cht98)) +
  geom_point(alpha=0.7, size=0.5) +
  theme_classic() +
  xlab("Elevation [m]") +
  ylab("Canopy height [m]") +
  theme(
      ## Strip labels/grid/background
      panel.grid=element_blank(),
      panel.grid.minor = element_line(),
      strip.background=element_blank(),
      ## text themes
      axis.text=element_text(size=9, face="plain"),
      axis.title=element_text(size=9, face="plain"),
      axis.title.x = element_text(vjust=-0.4),
      axis.title.y = element_text(vjust=1.4),
      axis.ticks.length=unit(.15, "cm"),
      axis.ticks.margin=unit(0.25, "cm")
  )

pdf("cht_elev.pdf", width=2.75591, height=2.75591)
plot(p2)
dev.off()

