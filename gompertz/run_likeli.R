### run_likeli.R --- 
## Filename: run_likeli.R
## Description: rerun zach's model with likelihood package
## Author: Noah Peart
## Created: Sat May 23 12:59:08 2015 (-0400)
## Last-Updated: Sat May 23 16:48:56 2015 (-0400)
##           By: Noah Peart
######################################################################
## source("~/work/allometry/gompertz/zach_setup.R")
abba <- readRDS("~/work/allometry/gompertz/temp/pp.rds")
names(abba)[which(names(abba) == "cht98")] <- "CHT98"

model1<-lm(CHT98~ELEV, data=abba)
model2<-lm(ELEV~CHT98, data=abba)
abba$R_CHT98<-abba$CHT98-(summary(model1)$coefficients[1]+summary(model1)$coefficients[2]*abba$ELEV)
abba$R_ELEV<-abba$ELEV-(summary(model2)$coefficients[1]+summary(model2)$coefficients[2]*abba$CHT98)

cor.test(abba$CHT98, abba$ELEV)
cor.test(abba$R_CHT98, abba$ELEV)
cor.test(abba$R_ELEV, abba$CHT98)

library(likelihood)

GOMPERTZ_CE<-function(D,C,E,beta,a1,b1,c1,d1,e1,f1,g1)
{
	(e1+beta*C+f1*E+g1*E*C)*exp(log(1.37/(e1+beta*C+f1*E+g1*E*C))*exp(-D*(a1+b1*E+c1*C+d1*C*E)))
}

var_C_RE<-list(D="DBH98", E="R_ELEV", C="CHT98")
var_C_RE$x<-"HTTCR98"
var_C_RE$mean<-"predicted"
var_C_RE$log<-TRUE

## par_GOMPERTZ_CE<-list(beta=0.95, a1=0.2, b1=0, c1=-0.005, d1=0, e1=2, f1=0, g1=0, sd=1)
par_GOMPERTZ_CE <- list(a1=0.199, b1=0.000251, c1=0.00511, d1=-0.0000141, 
           e1=2.31, f1=0.00410, beta=0.92, g1=-0.000271,
           sd=1)

par_hi_GOMPERTZ_CE<-list(beta=20, a1=8, b1=0.1, c1=1, d1=0.1, e1=150, f1=0.1, g1=0.1, sd=10)
par_lo_GOMPERTZ_CE<-list(beta=-20, a1=-8, b1=-0.1, c1=-1, d1=-0.1, e1=-150, f1=-0.1, g1=-0.1, sd=0.01)

## Fit
mle_GOMPERTZ_C_RE <- anneal(GOMPERTZ_CE, par_GOMPERTZ_CE, var_C_RE, abba, 
                            par_lo_GOMPERTZ_CE, par_hi_GOMPERTZ_CE, dnorm, 
                            "HTTCR98", hessian=T, max_iter=200000, initial_temp=10, 
                            temp_red=0.95)

ps <- mle_GOMPERTZ_C_RE$best_pars
mle_GOMPERTZ_C_RE$std_errs
mle_GOMPERTZ_C_RE$aic_corr

