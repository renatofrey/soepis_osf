### This script runs some analysis on the correlation between measures and runs an exploratory factor analysis
### (c) Renato Frey

set.seed(222)

library(polycor)
library(psych)
library(nFactors)
library(parallel)


load("../data/overview.Rdata")

# load risk-taking measures with imputed values
measures <- overview_imp[,dvs_risk]

# standardize measures
measures_std <- apply(measures, 2, scale)
dat <- measures_std

# compute (heterogeneous; if necessary) correlation matrix
c <- hetcor(dat)
cormat <- c$cor
nobs <- nrow(dat)
nvar <- ncol(dat)

# determine number of factors
ev <- eigen(cormat)
ap <- parallel(subject=nobs, var=nrow(cormat), rep=100, cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
print(nS)

# plot scree-plot?
if (F) {
  quartz()
  par(mar=c(5,5,2,2), las=1)
  plot(ev$values / length(ev$values), ylim=c(0,1), type="b")
  plot(nS, main="")
}

#nfact <- nS$Components$nparallel
nfact <- 3

# run exploratory factor analysis
m1 <- fa(r=cormat, covar=F, n.obs=nobs, nfactors=nfact, rotate="varimax", fm="minres")
m4 <- fa(r=cormat, covar=F, n.obs=nobs, nfactors=nfact+1, rotate="bifactor", fm="ml")