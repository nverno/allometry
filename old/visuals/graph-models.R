# graph the neighborhood models predicted points on top of observed points

# models that have worked for Moosilauke data
sr <- 2
spec <- "ABBA"
ind.var <- "priorba"
dep.var <- "bagrowth"
models <- c("slnm", "sldnm", "spnm", "spenm")

pred.neighbor <- sapply(models, function(x) {
    ps <- get.params(sr, spec, ind.var, dep.var, currentmodel = x)
    assign(x, do.call(x, list(ps, ind.var)))
})
pred.neighbor <- as.data.frame(pred.neighbor)
names(pred.neighbor) <- models
lm.fit <- lm(as.formula(paste(dep.var,"~",ind.var)), data = targets)
observed <- targets[,dep.var]
pred.lm <- predict(lm.fit)
xvar <- targets[,ind.var]

dat <- data.frame(pred.neighbor, lm = predict(lm.fit), observed = targets[,dep.var],
                  xvar = targets[,ind.var])
datlong <- melt(dat, id.vars = "xvar")
names(datlong) <- c("xvar","model","depvar")
datlong <- datlong[rev(order(datlong$model)),]

p1 <- ggplot(datlong, aes(xvar, depvar, color=model)) +
    geom_point(alpha=0.3, size=2)
p1

### for doug fir
sr <- 4
spec <- "FD"
ind.var <- "priorba"
dep.var <- "bagrowth"
models <- c("spm")
ps <- get.params(sr, spec, ind.var, dep.var, currentmodel = models)

pred.neighbor <- sapply(models, function(x) {
    ps <- get.params(sr, spec, ind.var, dep.var, currentmodel = x)
    assign(x, do.call(x, list(ps, ind.var)))
})
pred.neighbor <- as.data.frame(pred.neighbor)
names(pred.neighbor) <- models
lm.fit <- lm(as.formula(paste(dep.var,"~",ind.var)), data = targets)
observed <- targets[,dep.var]
pred.lm <- predict(lm.fit)
xvar <- targets[,ind.var]

dat <- data.frame(pred.neighbor, lm = predict(lm.fit), observed = targets[,dep.var],
                  xvar = targets[,ind.var])
datlong <- melt(dat, id.vars = "xvar")
names(datlong) <- c("xvar","model","depvar")
datlong <- datlong[rev(order(datlong$model)),]

p1 <- ggplot(datlong, aes(xvar, depvar, color=model)) +
    geom_point(alpha=0.3, size=2)
p1

p1 <- ggplot(datlong, aes(log(xvar), log(depvar), color=model)) +
    geom_point(alpha=0.3, size=2)
p1
