## Visualization platform for fitted MLE neighborhood models
## The idea is to keep track of what variables were used as well as
##  to visualize the data by plotting predicted over obs.
## Neighbor matrices are created according to radius used in the
##  fitting procedure, and most recent parameters for the model are
##  retrieved from 'parameters.csv'

## Data: i.e long-bc-derived.csv
dat <- read.csv("long-bc-derived.csv")
## Dependent variable, i.e. rGR
## What rGR? (i.e Envelope modeled to top performers in subsets
##  by SI and SDP)
dep.var <- "rgrsisdp"
rgrfactors1 <- "si"
rgrfactors2 <- "sdp"

## Neighbor model: "simplest"
currentmodel <- "simplest"

## SR: 6
sr <- 6

## Target species: Doug Firs
spec <- "FD"

## Neighbor variable: Prior Bole Volume
ind.var <- "priorbv"

## Related growth column (i.e. if 'priorbv' then choose 'bvgrowth')
growcol <- "bvgrowth"

## Fitting Algorithm: Nelder-Mead

## Make neighbor matrices
fit.MLE.models(dat, sr=sr, spec=spec, ind.var = ind.var,
               dep.var = dep.var, realdist = TRUE)

## Retrieve parameters of most recent fit
pars <- read.csv("parameters.csv")
ps <- get.params(sr,spec,ind.var,dep.var,currentmodel = currentmodel)
ps

## Predict NCI, assumes there is a NCI component in the model,
##  and the default parameters assume 'alpha' and 'beta' are the
##  relevant parameters
alpha = ps[["alpha"]]; beta = ps[["beta"]]
targets$nci <- rowSums(((bas ^ alpha)/(distances ^ beta)), na.rm=TRUE)

## Graph rGR vs NCI, Related growth column vs NCI
par(mfrow = c(1,2))
plot(targets$nci, targets[,dep.var])
plot(targets$nci, targets[,growcol])

## Graph predicted over obs. on graph of rGR vs ind.var
targets$predicted <- do.call(currentmodel, list(ps))
plot(targets[,ind.var], targets[,dep.var],
     main = simpleCap(paste(dep.var, "by", rgrfactors1, "and",
     rgrfactors2, "vs.", ind.var)), xlab = paste(ind.var),
     ylab = paste(dep.var))
points(targets[,ind.var], targets$predicted, col = "red")

## Print graph to pdf?  Will automatically create file name according
##  to the following convention: model,sr,ind.var, dep.var, combination
##  of variables to make rgr (if rgr is the dependent variable)
ifelse(length(grep("bv",ind.var))==0, ind.name <- "ba",
       ind.name <- "bv")
pdfname <- paste(currentmodel,sr,ind.name,"rgr",rgrfactors1,rgrfactors2,
                 sep="-")
pdf(paste0(pdfname,".pdf"))
plot(targets[,ind.var], targets[,dep.var],
     main = simpleCap(paste(dep.var, "by", rgrfactors1, "and",
     rgrfactors2, "vs.", ind.var)), xlab = paste(ind.var),
     ylab = paste(dep.var))
points(targets[,ind.var], targets$predicted, col = "red")
dev.off()




