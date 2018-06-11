### This script contains helper functions for the confirmatory factor analyses
### (c) Renato Frey

# load functions and libraries
library(lavaan)
library(semPlot)
require(reshape2)
require(ggplot2)

# some functions

get_mstring <- function(l, t=.3, bifactor=F, highord=F, schmid=F) {
  v <- row.names(l)
  
  if (bifactor == T) {
    Fs <- colnames(l)[-1]
    vars <- row.names(l)[which(apply(l >= t, 1, sum) >=1)]
    #vt <- v[(apply(abs(l[,Fs]), 1, max) >= t)]
    model <- paste("R =~ ", paste(v, collapse=" + "), sep="")
    
  }
  else if (schmid == T) {
    Fs <- colnames(l)[grepl("F", colnames(l))]
    model <- paste("R =~ ", paste(substr(Fs, 1, 2), collapse=" + "), sep="")
    #model <- paste("R =~ ", paste(row.names(l)[l$g >= t], collapse=" + "), sep="")
    #model <- paste("R =~ ", paste(v, collapse=" + "), sep="")
  }
  else if (highord == T) {
    Fs <- colnames(l)
    model <- paste("R =~ ", paste(paste("F", 1:length(Fs), sep=""), collapse=" + "), sep="")
  }
  else {
    model <- ""
    Fs <- colnames(l)
  }
  
  # on which factor does each measure have the highest loading?
  highest <- apply(l[,Fs], 1, which.max)
  
  for (i in 1:length(Fs)) {
    
    current <- l[,Fs[i]]
    
    # remove cross-loadings
    #current[i != highest] <- 0
    
    string <- paste("\nF", i, " =~ ", paste(names(which(abs(current) >= t)), collapse=" + "), sep="")
    model <- paste(model, string)
  }
  
  model <- paste(model, "\n")
  return(model)
}


get_fits <- function(f) {
  print(round(inspect(f, "fitmeasures")[c("cfi", "tli", "bic", "rmsea", "srmr", "npar", "df")], 3))  
}


compare <- function(fitA, fitB) {
  modelcomp <- anova(fitA, fitB)
  
  bicA <- BIC(fitA)
  bicB <- BIC(fitB)
  bf <- round(exp(-1/2 * (bicB - bicA)), 2)
  
  cfiA <- round(fitMeasures(fitA)["cfi"], 2)
  cfiB <- round(fitMeasures(fitB)["cfi"], 2)
  
  print(modelcomp)
  
  print(paste("CFI A:", cfiA, ", CFI B:", cfiB, ". CFI-Difference:", cfiB-cfiA))
  print(paste("BIC A:", round(bicA, 2), ", BIC B:", round(bicB, 2), ". BIC-Difference:", round(bicB-bicA, 2)))
  print(paste("Bayesfactor B | A:", bf))
  
}

ztrans <- function(x) {
  
  ztrans_vec <- function(vec) (vec - mean(vec, na.rm=TRUE)) / sd(vec, na.rm=TRUE)
  
  if (is.list(x)) result <- as.data.frame(apply(x, 2, ztrans_vec))
  else result <- ztrans_vec(x)
  return(result)
}

grps <- list(prop=dvs_risk[grepl("SOEP", dvs_risk)],
             beh=dvs_risk[!grepl("SOEP", dvs_risk)],
             freq=NA,
             latent=c("R", paste("F", 1:10, sep="")))
