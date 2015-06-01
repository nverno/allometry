### run_boot.R --- 
## Filename: run_boot.R
## Description: Helpers to run bootstrap for bootstrap.Rmd
## Author: Noah Peart
## Created: Wed May 27 16:15:43 2015 (-0400)
## Last-Updated: Sun May 31 18:02:24 2015 (-0400)
##           By: Noah Peart
######################################################################
run_boot <- function(dat, inds, ps, reps, update=FALSE, n=nrow(dat), elev="relev", 
                     dbh="DBH98", canht="canht", ht="HTTCR98", nPerClass=FALSE) {
    res <- unlist(ps)
    if (!nPerClass) n <- n / ceiling(length(inds))
    for (i in 1:reps) {
        ii <- unlist(lapply(inds, function(x) sample(x, n, replace=T)))
        samp <- dat[ii,]
        ## try({
        ## form <- as.formula(paste(ht, "~", "a*", dbh, "^b"))
        ## fit <- nls(HTTCR98 ~ a * DBH98 ^ b, start=list(a=0.5, b=0.1), data=samp)
        ## if (i %% 50 == 0) {
        ##     cat(paste("\nFitting with Simulated Annealing: iteration", i, "\nCurrent AIC(sample):", 
        ##               AIC(fit), "\n"))
        ##     fit <- run_fit(samp, ps, 98, method="SANN", maxit=2e6)
        ## }
        ## else
        fit <- run_fit(samp, ps, 98, method="Nelder-Mead", dbh=dbh, height=ht, canht=canht, elev=elev)
        ## }, silent = TRUE)
        res <- rbind(res, unlist(coef(fit)))
        ## if (alpha_beta) {
        ##     cs <- coef(fit)
        ##     alpha <- mean( cs[["a"]] + cs[["a1"]]*samp[,elev] + cs[["a2"]]*samp[,canht] + 
        ##                 cs[["a3"]]*samp[,elev]*samp[,canht] )
        ##     beta <- mean( cs[["b"]] + cs[["b1"]]*samp[,elev] + cs[["b2"]]*samp[,canht] + 
        ##                 cs[["b3"]]*samp[,elev]*samp[,canht] )
        ##     ab[i+1,] <- c(alpha=alpha, beta=beta)
        ## }
        if (update)
            ps <- coef(fit)
    }
    ## colnames(ab) <- c("alpha", "beta")
    ## attr(res, "ab") <- ab
    return(res)
}

## inds <- list(a=1:nrow(dat))
## tst1 <- run_boot(dat, inds=inds, ps=ps, reps=2, elev="relev", canht="cht98")
## tst2 <- run_boot(dat, inds=inds, ps=ps, reps=2, elev="relev", canht="cht98")
## runs <- list(run1=tst1, run2=tst2)
