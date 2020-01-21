### This script plots the specification curves
### (c) Renato Frey

library(viridis)
library(scales)

plot_sim <- T

my.cex = 1.125

load("../data/overview.Rdata")
dvs <- c(dvs_risk, "R", "REM", "REMsr", "REMbeh")

p_effs <- list.dirs("../objects/scicore/sca", full.names=F, recursive=F)
p_effs <- p_effs[p_effs != "sim"]
p_effs <- p_effs[p_effs != "incomenet"]

col_null <- alpha("black", .1)
col_spec <- inferno(20)[3]

col_pos <- viridis(20)[18]
col_neg <- inferno(20)[14]
col_neu <- viridis(20)[7]

cors <- NULL

for (p_eff in p_effs) {
  
  print(p_eff)
  
  results <- read.csv(paste("../data/sca/results_", p_eff, ".csv", sep=""))
      
  results$col <- "black"
  results$col[which(results$rstan_est > 0 & results$rstan_HDIl > 0)] <- col_pos
  results$col[which(results$rstan_est > 0 & results$rstan_HDIl < 0)] <- col_neu
  results$col[which(results$rstan_est < 0 & results$rstan_HDIu > 0)] <-  col_neu
  results$col[which(results$rstan_est < 0 & results$rstan_HDIu < 0)] <- col_neg
  
  p_eff_lab <- p_eff
  if (p_eff == "sex") p_eff_lab <- "sex (female)"
  if (p_eff == "fluid") p_eff_lab <- "fluid intelligence"
  if (p_eff == "cryst") p_eff_lab <- "cryst. intelligence"
  if (p_eff == "hhinc") p_eff_lab <- "household income"
  if (p_eff == "incomenet") p_eff_lab <- "personal income"
  if (p_eff == "eduyears") p_eff_lab <- "years of education"
  
  
  p_vars <- c("rstan_est", "rstan_r2")[1]
  
  for (p_var in p_vars) {
    
    if (p_var == "rstan_est") {
      pdf(file=paste("../output/sca_", p_eff, ".pdf", sep=""), width=10, height=8)
      y.lab <- paste("Effect of ", p_eff_lab, "\non risk preference")
      #y.lab <- p_eff_lab
      y.lim <- c(-.6, .6)
      ord.decr <- T
    }
    
    if (p_var == "rstan_r2") {
      pdf(file=paste("../output/scaR2_", p_eff, ".pdf", sep=""))
      y.lab <- paste("R^2 (", p_eff, " + covariates)", sep="")
      y.lim <- c(0, .4)
      ord.decr <- F
    }
    
    # sort by effect size
    results <- results[order(results[,p_var], decreasing=ord.decr),]
    n <- nrow(results)
    
    # generate data.frame for specifications
    specs <- t(results)
    
    specs <- rbind(specs[c("hhinc", "sex", "age", "fluid", "cryst", "eduyears", "empl", "social", "sports"),],
                   "NA"=NA,
                   specs[dvs,])
    
    specs[specs == "no"] <- 0
    specs[specs == "yes"] <- 1
    specs[p_eff,] <- 1
    specs <- apply(specs, c(1,2), as.numeric)
    specs[specs == 0] <- NA
    specs[specs == 1] <- "|"
    specs <- specs[nrow(specs):1,]
    
    ind_currdv <- which(row.names(specs) == p_eff)
    
    row.names(specs) <- gsub("eduyears", "Years of education", row.names(specs))
    row.names(specs) <- gsub("empl", "Employment status", row.names(specs))
    row.names(specs) <- gsub("social", "Time use (social)", row.names(specs))
    row.names(specs) <- gsub("sports", "Time use (sports)", row.names(specs))
    row.names(specs) <- gsub("hhinc", "Household income", row.names(specs))
    row.names(specs) <- gsub("incomenet", "Personal income", row.names(specs))
    row.names(specs) <- gsub("cryst", "Crystallized intelligence", row.names(specs))
    row.names(specs) <- gsub("fluid", "Fluid intelligence", row.names(specs))
    row.names(specs) <- gsub("age", "Age", row.names(specs))
    row.names(specs) <- gsub("Agesqr", "Age^2", row.names(specs))
    row.names(specs) <- gsub("sex", "Sex", row.names(specs))
    
    
    row.names(specs) <- gsub("SOEPgen", "General", row.names(specs))
    row.names(specs) <- gsub("SOEPdri", "Driving", row.names(specs))
    row.names(specs) <- gsub("SOEPinv", "Investment", row.names(specs))
    row.names(specs) <- gsub("SOEPrec", "Recreation", row.names(specs))
    row.names(specs) <- gsub("SOEPocc", "Occupation", row.names(specs))
    row.names(specs) <- gsub("SOEPhea", "Health", row.names(specs))
    row.names(specs) <- gsub("SOEPsoc", "Social", row.names(specs))
    
    row.names(specs) <- gsub("S.dfe2", "Sample size (DFE2)", row.names(specs))
    row.names(specs) <- gsub("S.dfe4", "Sample size (DFE4)", row.names(specs))
    
    row.names(specs) <- gsub("R.dfe2", "Risky choice (DFE2)", row.names(specs))
    row.names(specs) <- gsub("R.dfe4", "Risky choice (DFE4)", row.names(specs))
    row.names(specs) <- gsub("R.dfd2", "Risky choice (DFD2)", row.names(specs))
    row.names(specs) <- gsub("R.dfd4", "Risky choice (DFD4)", row.names(specs))
    
    row.names(specs) <- gsub("\\<R\\>", "Psychom. mod.: 'R'", row.names(specs))
    row.names(specs) <- gsub("\\<REM\\>", "Statist. mod.: All", row.names(specs))
    row.names(specs) <- gsub("REMsr", "Statist. mod.: Prop.", row.names(specs))
    row.names(specs) <- gsub("REMbeh", "Statist. mod.: Beh.", row.names(specs))
    
    
    
    
    
    layout(matrix(c(1,2,2), ncol = 1))
    
    par(mar=c(0,14,2,0), mgp=c(4,1,0))
    
    xs <- seq(1, n, length.out=6)
    x.lab = -10
    
    # generate main plot (upper panel)
    plot(results[,p_var], las=1, xaxt="n", xlab="", ylab="", type="n", ylim=y.lim, yaxt="n", cex.lab=my.cex*.75, frame=F)
    
    ord1 <- order(results$sim_lm_max, decreasing=T)
    ord2 <- order(results$sim_lm_min, decreasing=F)
    polygon(x = c(1:n, n:1), y = c(results[ord1, "sim_lm_max"], results[ord2, "sim_lm_min"]), col="lightgrey", border=0)
    lines(x = 1:n, y=results[ord1, "sim_lm_max"], lty=1, col="darkgrey")
    lines(x = n:1, y=results[ord2, "sim_lm_min"], lty=1, col="darkgrey")
    
    rx <- -1300
    rx2 <- 4520
    rect(rx, -.64, rx2, .785, lwd=2, xpd=T)
    rect(rx, -.64, rx+200, .785, lwd=2, xpd=T, col="black")
    text(rx+140, .08, "Specification curve", col="white", srt=90, xpd=T, cex=1.5)
    
    text(-650, 0.05, y.lab, srt=90, pos=1, xpd=T, cex=my.cex*1.4)
    
    text(x.lab, y=0.7, "Model specfications", pos=2, xpd=T, cex=my.cex)
    #text(n/2, y=0.8, "Model specfications", pos=1, xpd=T, cex=my.cex)
    text(x=xs, y=0.7, round(xs), xpd=T, cex=my.cex)
    
    #axis(1, at=xs, label=NA, lwd=0, lwd.ticks=1, col="lightgrey")
    abline(v=xs, col="lightgrey")
    if (p_var == "rstan_est") axis(2, pos=0, at=seq(1, -1, by=-.25), las=1, cex.axis=my.cex)
    if (p_var == "rstan_r2") axis(2, at=seq(0, 1, by=.1), las=1)
    
    # loop through specifciations
    for (i in 1:n) {
      if (p_var == "rstan_est") {
        p_col <- alpha(results$col[i], .75)
        
        # plot confidence intervals
        if (p_var == "rstan_est" & i %% 1 == 0) lines(x=cbind(i, i), y=cbind(results[i,"rstan_HDIl"], results[i,"rstan_HDIu"]), col=p_col, lwd=.1)
        
        # add frequentist estimates?  
        if (F & p_var == "rstan_est") {
          points(i, results[i,"lm_est"] - results[i,"lm_stderr"], pch=20, cex=.1)
          points(i, results[i,"lm_est"] + results[i,"lm_stderr"], pch=20, cex=.1)
        }
        # lines(x=cbind(i, i), y=cbind(results[i,"lm_est"]+results[i,"lm_stderr"], results[i,"lm_est"]-results[i,"lm_stderr"]), col=p_col, lwd=0.1)
        
        # plot Null-distribution? => order by max simulated effect size
        if (plot_sim == T  & i %% 1 == 0) {
          ind_sim <- floor(seq(1, nrow(results), length.out=sum(!is.na(results$sim_lm_max))))

        }
        
        
      }
    }

    lines(x=c(0, nrow(results)), y=c(0,0), lwd=1, lty=2)
    
    # plot specification curve
    ind1 <- seq(1, length(results[,"rstan_est"]), by=10)
    if (p_var == "rstan_est") points(ind1, results[,p_var][ind1], col=col_spec, type="l", lwd=6, xpd=F)
    if (p_var == "rstan_r2") points(results[,p_var], pch=15, cex=.4)
    
    # add frequentist specification curve?
    ind2 <- seq(1, length(results[,"lm_est"]), by=10)
    if (T & p_var == "rstan_est") points(ind2, results[,"lm_est"][ind2], col="white", type="l", lwd=1.5)
    
    # save correlation between Bayesian and freqentist estimates
    cors <- c(cors, cor(results[,"rstan_est"], results[,"lm_est"]))
    
    # thinning out
    specs[,seq(1, ncol(specs), by=3)] <- NA
    specs[,seq(2, ncol(specs), by=3)] <- NA
    
    par(mar=c(0,14,0,0), mgp=c(3,1,0))
    
    # generate specification table (lower panel)
    plot(NA, xlim=c(1, n), ylim=c(1, nrow(specs)), las=1, xlab="Models (ordered by effect size)", ylab="", yaxt="n", xaxt="n", frame=F, cex=my.cex)
    
    rect(rx, 0, rx2, 28.25, lwd=2, xpd=T)
    rect(rx, 0, rx+200, 28.25, lwd=2, xpd=T, col="black")
    text(rx+140, 13.65, "Specification panel", col="white", srt=90, xpd=T, cex=1.5)
    
    #text(x=xs, y=nrow(specs)+1.3, round(xs), xpd=T, cex=my.cex)
    #text(x.lab, nrow(specs)+1.3, "Model", pos=2, xpd=T, cex=my.cex)
    for (i in 1:nrow(specs)) {
      if (row.names(specs)[i] == "NA") next
      if (is.element(row.names(specs)[i], c("Predictors", "Dependent var."))) fw <- 2 else fw <- 1
      text(x.lab, i, row.names(specs)[i], xpd=T, pos=2, font=fw, cex=my.cex)
      if (is.element(row.names(specs)[i], c("Predictors", "Dependent var."))) next
      if (i %% 2 == 0) rect(0, i-.5, n, i+.5, col=gray(.95), border=F)
      if (i %% 2 != 0) rect(0, i-.5, n, i+.5, col=gray(.90), border=F)
      
      if (i == ind_currdv) col_ticks <- "red" else col_ticks <- "red"
      points(x=1:n, y=rep(i, n), pch=specs[i,], cex=.5, col=col_ticks)
    }
    
    abline(v=xs, col="lightgrey", xpd=F)
    
    x.lab2 <- -925
    x.lab3 <- -900
    
    text(x.lab2, 22.5, "Independent variables", srt=90, xpd=T, pos=3, cex=my.cex*1.25)
    lines(x=c(x.lab3, x.lab3), y=c(18.75,27.25), lwd=.5, xpd=T)
    
    text(x.lab2, 8.5, "Dependent variables", srt=90, xpd=T, pos=3, cex=my.cex*1.25)
    lines(x=c(x.lab3, x.lab3), y=c(.75,17.25), col="darkgrey", xpd=T)
    
    
    
    x.lab4 <- -775
    x.lab5 <- -750
    text(x.lab4, 13.5, "Propensity meas.", srt=90, xpd=T, pos=3, cex=my.cex*1)
    lines(x=c(x.lab5, x.lab5), y=c(10.75,17.25), lwd=.5, xpd=T)
    
    text(x.lab4, 7.25, "Behavioral meas.", srt=90, xpd=T, pos=3, cex=my.cex*1)
    lines(x=c(x.lab5, x.lab5), y=c(4.75,10.25), lwd=.5, xpd=T)
    
    text(x.lab4, 2.25, "Summary", srt=90, xpd=T, pos=3, cex=my.cex*1)
    lines(x=c(x.lab5, x.lab5), y=c(.75,4.25), lwd=.5, xpd=T)
    
    abline(h=18, lwd=1, lty=3, xpd=T)
    
    
    dev.off()
    
  }
  
}

print("Correlations between Bayesian and freq. estimates:")
print(cors)
print(round(mean(cors), 4))