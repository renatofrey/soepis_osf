### This script plots the specification curves
### (c) Renato Frey

plot_sim <- T

my.cex = 1.125

load("../data/overview.Rdata")
dvs <- c(dvs_risk, "R", "REM", "REMsr", "REMbeh")

p_effs <- list.dirs("../objects/scicore/sca", full.names=F, recursive=F)
p_effs <- p_effs[p_effs != "sim"]

p_effs <- p_effs[p_effs != "incomenet"]

col_null <- cbind(t(col2rgb("darkgrey")), alpha=150)
col_null <- rgb(col_null[,1], col_null[,2], col_null[,3], col_null[,4], maxColorValue=255)

for (p_eff in p_effs) {
  
  print(p_eff)
  
  results <- read.csv(paste("../data/sca/results_", p_eff, ".csv", sep=""))
  
  results$col <- "black"
  results$col[which(results$mean > 0 & results$HDIl > 0)] <- "lightgreen"
  results$col[which(results$mean > 0 & results$HDIl < 0)] <- "darkgreen"
  results$col[which(results$mean < 0 & results$HDIu > 0)] <- "darkred"
  results$col[which(results$mean < 0 & results$HDIu < 0)] <- "red"
  
  p_eff_lab <- p_eff
  if (p_eff == "fluid") p_eff_lab <- "fluid intelligence"
  if (p_eff == "cryst") p_eff_lab <- "crystallized intelligence"
  if (p_eff == "hhinc") p_eff_lab <- "household income"
  if (p_eff == "incomenet") p_eff_lab <- "personal income"
  if (p_eff == "eduyears") p_eff_lab <- "years of education"
  
  for (p_var in c("mean", "r2_med")) {
    
    if (p_var == "mean") {
      pdf(file=paste("../output/sca_", p_eff, ".pdf", sep=""), width=8, height=8)
      y.lab <- paste("Effect of \n", p_eff_lab)
      #y.lab <- p_eff_lab
      y.lim <- c(-.6, .6)
      ord.decr <- T
    }
    
    if (p_var == "r2_med") {
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
    
    specs <- rbind(specs[c("sex", "age", "fluid", "cryst", "eduyears", "hhinc", "empl", "social", "sports"),],
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
    row.names(specs) <- gsub("empl", "Employment stat.", row.names(specs))
    row.names(specs) <- gsub("social", "Time use (social)", row.names(specs))
    row.names(specs) <- gsub("sports", "Time use (sports)", row.names(specs))
    row.names(specs) <- gsub("hhinc", "Household income", row.names(specs))
    row.names(specs) <- gsub("incomenet", "Personal income", row.names(specs))
    row.names(specs) <- gsub("cryst", "Cryst. intelligence", row.names(specs))
    row.names(specs) <- gsub("fluid", "Fluid intelligence", row.names(specs))
    row.names(specs) <- gsub("age", "Age", row.names(specs))
    row.names(specs) <- gsub("Agesqr", "Age^2", row.names(specs))
    row.names(specs) <- gsub("sex", "Sex", row.names(specs))

    
    layout(matrix(c(1,2,2), ncol = 1))
    
    par(mar=c(0,10,2,1), mgp=c(4,1,0))
    
    xs <- seq(1, n, length.out=6)
    x.lab = -100
    
    # generate main plot (upper panel)
    plot(results[,p_var], las=1, xaxt="n", xlab="", ylab="", type="n", ylim=y.lim, yaxt="n", cex.lab=my.cex*.75, frame=F)
    
    text(-700, 0, y.lab, srt=90, pos=1, xpd=T, cex=my.cex*1.5)
    
    text(x.lab, y=0.7, "Model", pos=2, xpd=T, cex=my.cex)
    text(x=xs, y=0.7, round(xs), xpd=T, cex=my.cex)
    
    #axis(1, at=xs, label=NA, lwd=0, lwd.ticks=1, col="lightgrey")
    abline(v=xs, col="lightgrey")
    if (p_var == "mean") axis(2, pos=0, at=seq(1, -1, by=-.25), las=1, cex.axis=my.cex)
    if (p_var == "r2_med") axis(2, at=seq(0, 1, by=.1), las=1)
    
    # loop through specifciations
    for (i in 1:n) {
      if (p_var == "mean") {
        p_col <- cbind(t(col2rgb(results$col[i])), alpha=50)
        p_col <- rgb(p_col[,1], p_col[,2], p_col[,3], p_col[,4], maxColorValue=255)
        lines(x=cbind(i, i), y=cbind(results[i,"HDIl"], results[i,"HDIu"]), col=p_col, lwd=0.1)
        # plot Null-distribution?
        if (plot_sim == T) {
          ind_sim <- floor(seq(1, nrow(results), length.out=sum(!is.na(results$sim_max))))
          lines(x=cbind(ind_sim[i], ind_sim[i]), y=cbind(results[order(results$sim_min, decreasing=T)[i],"sim_min"], results[order(results$sim_max, decreasing=T)[i],"sim_max"]), col=col_null, lwd=1)
        }
        
      }
      if (p_var == "r2_med") {
        v1 <- results[i,"r2_med"] + results[i,"r2_sd"]
        v2 <- results[i,"r2_med"] - results[i,"r2_sd"]
        lines(x=cbind(i, i), y=cbind(v1, v2), col=gray(.9))
      }
    }
    
    if (p_var == "mean") {
      #points(results$sim_median[order(results$sim_median, decreasing=T)], col="black", type="l", lwd=1)
      #points(results$sim_min[order(results$sim_min, decreasing=T)], col="black", cex=.4, pch=16, type="l", lty=3, lwd=1)
      #points(results$sim_max[order(results$sim_max, decreasing=T)], col="black", cex=.4, pch=16, type="l", lty=3, lwd=1)
    }
    
    lines(x=c(0, nrow(results)), y=c(0,0), lwd=1, lty=2)
    
    if (p_var == "mean") points(results[,p_var], pch=20, cex=.2, col="black")
    if (p_var == "r2_med") points(results[,p_var], pch=15, cex=.4)

    
    # thinning out
    specs[,seq(1, ncol(specs), by=3)] <- NA
    specs[,seq(2, ncol(specs), by=3)] <- NA
    
    par(mar=c(1,10,0,1), mgp=c(3,1,0))
    
    # generate specification table (lower panel)
    plot(NA, xlim=c(1, n), ylim=c(1, nrow(specs)), las=1, xlab="Models (ordered by effect size)", ylab="", yaxt="n", xaxt="n", frame=F, cex=my.cex)
    #text(x=xs, y=nrow(specs)+1.3, round(xs), xpd=T, cex=my.cex)
    #text(x.lab, nrow(specs)+1.3, "Model", pos=2, xpd=T, cex=my.cex)
    for (i in 1:nrow(specs)) {
      if (row.names(specs)[i] == "NA") next
      if (is.element(row.names(specs)[i], c("Predictors", "Dependent var."))) fw <- 2 else fw <- 1
      text(x.lab, i, row.names(specs)[i], xpd=T, pos=2, font=fw, cex=my.cex)
      if (is.element(row.names(specs)[i], c("Predictors", "Dependent var."))) next
      if (i %% 2 == 0) rect(-100, i-.5, n+100, i+.5, col=gray(.95), border=F)
      if (i %% 2 != 0) rect(-100, i-.5, n+100, i+.5, col=gray(.90), border=F)
      
      if (i == ind_currdv) col_ticks <- "goldenrod1" else col_ticks <- "blue"
      points(x=1:n, y=rep(i, n), pch=specs[i,], cex=.5, col=col_ticks)
    }
    
    abline(v=xs, col="lightgrey", xpd=F)

    text(-1900, 21.5, "Predictors", srt=90, xpd=T, pos=3, cex=my.cex*1.5)
    text(-1900, 8, "Dependent variables", srt=90, xpd=T, pos=3, cex=my.cex*1.5)
    
    dev.off()
    
  }
  
}

if (T) {
  system("pdfjam ../output/sca_sex.pdf ../output/sca_age.pdf ../output/sca_fluid.pdf ../output/sca_cryst.pdf --nup 2x2 --outfile ../output/sca.pdf")
  system("pdfcrop ../output/sca.pdf ../output/sca.pdf")
  
  system("pdfjam ../output/sca_sex.pdf ../output/sca_age.pdf ../output/sca_fluid.pdf ../output/sca_cryst.pdf --nup 2x2 --outfile ../output/sca1.pdf")
  system("pdfcrop ../output/sca1.pdf ../output/sca1.pdf")
  
  system("pdfjam ../output/sca_eduyears.pdf ../output/sca_hhinc.pdf --nup 2x1 --outfile ../output/sca2.pdf")
  system("pdfcrop ../output/sca2.pdf ../output/sca2.pdf")
}