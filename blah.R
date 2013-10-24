bclong <- read.csv("long-bc-derived.csv")





## ratio of bvgrowth/priorbv using dflong
max(dflong$priorbv, na.rm=TRUE)
ggplot(dflong, aes(priorbv)) + geom_histogram()
dflong <- subset(dflong, priorbv< 5)

## si by class
levels(dflong$si)
table(dflong$si)
dflong$si <- as.numeric(dflong$si)
dflong$siclass <- cut(dflong$si, breaks = c(0,4,13,20))

ggplot(dflong, aes(priorbv, bvgrowth, color = siclass, shape = sdpclass)) +
    geom_point(alpha = 0.4)

ggplot(dflong, aes(priorbv, bvgrowth, color = sdpclass)) +
    geom_point(alpha = 0.4) + facet_wrap(~siclass)

dflong$ratio <- dflong$bvgrowth/dflong$priorbv
dflong <- dflong[dflong$ratio < 100,]
dfonly <- dflong[dflong$spec == "FD",]
ggplot(dfonly, aes(y = ratio, x = si, fill = sdpclass)) + geom_boxplot()


## install 1, plot total bole volume against plot density
dfplot <- read.csv("sdp-plot-level.csv")
dfplot$pplot <- factor(dfplot$pplot)
install1 <- subset(dfplot, install == 1)
tst = dfplot[dfplot$install %in% c(10,11),]

ggplot(dfplot[dfplot$install %in% c(72),],
       aes(log(plotden), log(plotbv), shape = factor(pplot),color = pplot)) + geom_point() +
    facet_wrap(~install, nrow = 3)

ggplot(install1, aes(plotden, plotbv, shape = pplot, color = pplot)) + geom_point() +
    labs(title="Install 1")

ggplot(dfplot[dfplot$install==2,], aes(plotden, plotbv, shape = pplot, color = pplot)) + geom_point() +
    labs(title="Install 2")

ggplot(dfplot[dfplot$install==3,], aes(plotden, plotbv, shape = pplot, color = pplot)) + geom_point() +
    labs(title="Install 3")


## using original data
den1 <- nrow(subset(orig, stat1=="ALIVE" & spp=="FD" & install==1 & plot==10))/
    mean(subset(orig, stat1=="ALIVE" & spp=="FD" & install==1 & plot==10)$pltarea)

den2 <- nrow(subset(dflong, stat=="ALIVE" & spec=="FD" & install==1 & plot==10 &
                    time==73))/
    as.numeric(levels(droplevels(subset(dflong, stat=="ALIVE" & spec=="FD" &
                                        time==73 & install==1 & plot==10)$pltarea)))

## for some reason, I am missing the first tree (tag 8) in the long data...
tree1 <- subset(orig, stat1=="ALIVE" & spp=="FD" & install==1 & plot==10)$tree
tree2 <- subset(dflong, stat=="ALIVE" & spec=="FD" & install==1 & plot==10 &
                    time==73)$tag
all(tree1[2:length(tree1)]==tree2)

missing <- subset(orig, stat1=="ALIVE" & spp=="FD" & install==1 & plot==10 & tree == 8)





