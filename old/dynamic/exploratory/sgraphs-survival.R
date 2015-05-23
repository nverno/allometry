## create S-graphs for Moosilauke species with paths colored by survival
## height/canopy height vs dbh
library(ggplot2)
library(grid)

canhts <- read.csv("~/work/data/data/boles/canhts.csv")
pp <- read.csv("~/work/data/data/dynamicallometry/moose-long-canopies.csv")

## add survival indicator column, if tree died at any time during census
##  it gets a 1
pp$died <- ifelse(!is.na(pp$yrmort), 1, 0)

## paths colored by survival
tst <- pp[is.na(pp$eht) & pp$spec %in% c("ABBA", "PIRU") & pp$elevcl == "M" ,]#& pp$aspcl == "E",]
ggplot(tst, aes(dbh, ht/canht, group = id, col = died)) +
    geom_path(arrow = arrow(), alpha = 0.5) + geom_point(alpha = 0.5) +
    facet_wrap(~spec + aspcl, ncol = 2) + ggtitle("multiple species") + geom_hline(yintercept=1, lty = 2)
    #xlim(0,5)

## plot survival sgraphs interactively, printout prints graphs to file as option
plots <- unique(pp$pplot); plots <- plots[plots > 3]
printout = FALSE;
pause = TRUE;
specs = c("ABBA")

for(i in plots) {
    x <- droplevels(subset(pp, pplot == i & spec %in% specs & is.na(eht)))
    if (nrow(x) == 0)
        next;
    ## Start plotting stuff
    print(ggplot(x, aes(dbh, ht/canht, group = id, col = died)) +
          geom_path(arrow = arrow(), alpha = 0.5) + geom_point(alpha = 0.5) +
          facet_wrap(~spec + aspcl + elevcl, ncol = 2) + ggtitle(paste(paste(specs, collapse = ", "), "in plot", i)) +
          geom_hline(yintercept=1, lty = 2))
    if (pause == TRUE)
        readline("Enter to continue")
    flush.console()
    if (printout == TRUE) {
        ggsave(filename = paste0("~/work/dynamicallometry/visuals/sgraphs/survival/","plot-",unique(x$pplot),
               "-spp-",paste(specs, collapse = "-"),".pdf"))
        dev.off()
    }
}

tst <- subset(pp, spec == "ABBA" & pplot == 4 & is.na(eht))
ggplot(tst, aes(dbh, ht/canht, group = id, col = died)) +
          geom_path(arrow = arrow(), alpha = 0.5) + geom_point(alpha = 0.5) +
          facet_wrap(~spec + aspcl + elevcl, ncol = 2) + ggtitle(paste(paste(specs, collapse = ", "), "in plot", i)) +
          geom_hline(yintercept=1, lty = 2) + xlim(0, 4) + ylim(0, 0.5)
 ##    scatter.smooth(x$priorbv, x$res.top, lpars = list(col="red"),
 ##                   main = paste("Residuals vs Fitted",levels(x$sdpclass),mean(x$si)))
 ##    abline(h = 0, lty = 2)
 ##    plot(x$priorbv, x$bvgrowth, main = "BV growth vs. Prior BV with Predicted")
 ## ## curve(reg.a * x ^ reg.b, add=TRUE, col="blue", lwd=2)
 ##    curve(top.a * x ^ top.b, add=TRUE, col="green", lwd=2)
 ##    hist(x$res.top, main = "Distribution of the residuals")
 ##    abline(v = 1, lty = 2)
 ##    if(length(x$numnebs[!is.na(x$numnebs)])>2) {
 ##        plot(x$numnebs, x$rgr.top, main = "rGR vs Neighbor Density")
 ##    abline(h=1, lty=2);
 ##    tryCatch(abline(lm(x$rgr.top ~ x$numnebs), col="blue"),
 ##             error=function(e) NULL )
 ##    }

