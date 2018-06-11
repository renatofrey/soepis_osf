### This script implements the confirmatory factor analysis (bifactor model)
### (c) Renato Frey

# some exploratory factor analyis to identify the latent variables
source("efa.R")

set.seed(777)

library(psych)
source("cfa_load.R")

ths <- .2


# assemble correct model string
sel_bif <- T
sel_model <- m4
str <- get_mstring(sel_model$loadings, bifactor=sel_bif, t=ths)
cat(str)

fit <- lavaan::cfa(str, data=measures_std, sample.nobs=nobs, orthogonal=T, std.lv=T)

fits <- get_fits(fit)
p_fits <- paste(paste(names(fits), fits, sep="="), collapse=" / ")
p_fits <- ""
pdf(file=paste("../output/bifactor.pdf", sep=""), width=9, height=6)
source("p_bifactor.R")
dev.off()

# save factor values
if (T) {
  vars_mani <- row.names(fitted.values(fit)$cov)
  lats <- c("R", paste("F", 1:99, sep=""))
  vars_lat <- lats[is.element(lats, parameterEstimates(fit)$lhs)]
  dat_ok <- na.omit(dat[,vars_mani])
  pred <- matrix(NA, nrow=nrow(dat), ncol=length(vars_lat))
  colnames(pred) <- vars_lat
  row.names(pred) <- row.names(dat)
  pred[,] <- predict(fit, newdata=dat)
  phenotypes <- cbind(dat, pred)
  
  save(vers, str, fit, pred, get_fits, compare, file="../data/cfa.Rdata")
}