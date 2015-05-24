pp<-read.csv("trees.csv")
pp<-subset(pp, subset=(is.na(HT98)==F & HT98>0))

for(i in 1:nrow(pp))
{
	pp$CHT98[i]<-max(pp$HT98[pp$PPLOT==pp$PPLOT[i] & pp$BQUDX>=pp$BQUDX[i]-1 & pp$BQUDX<=pp$BQUDX[i]+1 & pp$BQUDY>=pp$BQUDY[i]-1 & pp$BQUDY<=pp$BQUDY[i]+1])
}

pp<-subset(pp, subset=(DBH98>0 & is.na(DBH98)==F & is.na(CHT98)==F & BQUDX>1 & BQUDX<10 & BQUDY>1 & BQUDY<10))

abba<-subset(pp, subset=(SPEC=="ABBA"))

library(likelihood)

model1<-lm(CHT98~ELEV, data=abba)
model2<-lm(ELEV~CHT98, data=abba)

abba$R_CHT98<-abba$CHT98-(summary(model1)$coefficients[1]+summary(model1)$coefficients[2]*abba$ELEV)
abba$R_ELEV<-abba$ELEV-(summary(model2)$coefficients[1]+summary(model2)$coefficients[2]*abba$CHT98)

cor.test(abba$R_CHT98, abba$ELEV)
cor.test(abba$R_ELEV, abba$CHT98)

############################################################

var<-list(D="DBH98")
var$x<-"HT98"
var$mean<-"predicted"
var$log<-TRUE

var_C<-list(D="DBH98", C="CHT98")
var_C$x<-"HT98"
var_C$mean<-"predicted"
var_C$log<-TRUE

var_E<-list(D="DBH98", E="ELEV")
var_E$x<-"HT98"
var_E$mean<-"predicted"
var_E$log<-TRUE

var_C_RE<-list(D="DBH98", E="R_ELEV", C="CHT98")
var_C_RE$x<-"HT98"
var_C_RE$mean<-"predicted"
var_C_RE$log<-TRUE

var_RC_E<-list(D="DBH98", E="ELEV", C="R_CHT98")
var_RC_E$x<-"HT98"
var_RC_E$mean<-"predicted"
var_RC_E$log<-TRUE

############################################################

#######################################
########### GOMPERTZ MODELS ###########
#######################################

GOMPERTZ<-function(D,a1,q1)
{
	q1*exp(log(1.37/q1)*exp(-D*a1))
}

par_GOMPERTZ<-list(q1=15, a1=0.5, sd=1)
par_hi_GOMPERTZ<-list(q1=40, a1=5, sd=10)
par_lo_GOMPERTZ<-list(q1=1, a1=0.01, sd=0.01)


GOMPERTZ_C<-function(D,C,beta,a1,b1,c1)
{
	(c1+beta*C)*exp(log(1.37/(c1+beta*C))*exp(-D*(a1+b1*C)))
}

par_GOMPERTZ_C<-list(beta=1, a1=0.5, b1=-0.01, c1=5, sd=1)
par_hi_GOMPERTZ_C<-list(beta=2, a1=5, b1=10, c1=20, sd=10)
par_lo_GOMPERTZ_C<-list(beta=0.2, a1=0.01, b1=-10, c1=-20, sd=0.01)


GOMPERTZ_E<-function(D,E,beta,a1,b1,c1)
{
	(c1+beta*E)*exp(log(1.37/(c1+beta*E))*exp(-D*(a1+b1*E)))
}

par_GOMPERTZ_E<-list(beta=-0.0001, a1=0.5, b1=-0.01, c1=15, sd=1)
par_hi_GOMPERTZ_E<-list(beta=0.1, a1=5, b1=10, c1=80, sd=10)
par_lo_GOMPERTZ_E<-list(beta=-0.1, a1=-5, b1=-10, c1=-80, sd=0.01)


GOMPERTZ_CE<-function(D,C,E,beta,a1,b1,c1,d1,e1,f1,g1)
{
	(e1+beta*C+f1*E+g1*E*C)*exp(log(1.37/(e1+beta*C+f1*E+g1*E*C))*exp(-D*(a1+b1*E+c1*C+d1*C*E)))
}

par_GOMPERTZ_CE<-list(beta=0.95, a1=0.2, b1=0, c1=-0.005, d1=0, e1=2, f1=0, g1=0, sd=1)
par_hi_GOMPERTZ_CE<-list(beta=20, a1=8, b1=0.1, c1=1, d1=0.1, e1=150, f1=0.1, g1=0.1, sd=10)
par_lo_GOMPERTZ_CE<-list(beta=-20, a1=-8, b1=-0.1, c1=-1, d1=-0.1, e1=-150, f1=-0.1, g1=-0.1, sd=0.01)


####

mle_GOMPERTZ_C_RE<-anneal(GOMPERTZ_CE, par_GOMPERTZ_CE, var_C_RE, abba, par_lo_GOMPERTZ_CE, par_hi_GOMPERTZ_CE, dnorm, "HT98", hessian=T, max_iter=200000, initial_temp=10, temp_red=0.9)



mle_GOMPERTZ_C_RE$best_pars
mle_GOMPERTZ_C_RE$std_errs
mle_GOMPERTZ_C_RE$aic_corr



