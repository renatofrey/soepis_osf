### This script plots the distributions of predictor and dependent variables
### (c) Renato Frey

load("../data/overview.Rdata")
load("../data/cfa.Rdata")
overview$R <- pred[,"R"]


p_cols <- c(iv="red", dv="blue")

pdf(file="../output/distributions.pdf", height=10, width=8)
par(mfrow=c(6, 4), mar=c(3,3,3,1))

dvs_risk <- c(dvs_risk, "R")
ivs <- c("sex", "age", "fluid", "cryst", "hhinc", "eduyears", "social", "sports", "empl")

vars <- c(ivs, dvs_risk)

for (v in vars) {
 
  dat <- overview[,v]
  miss <- sum(is.na(dat))
  if (is.na(labels[v])) lab <- v else lab <- labels[v]
  p_title <- lab
  
  if (is.element(v, dvs_risk)) grp <- "dv"
  if (is.element(v, ivs)) grp <- "iv"
   
  if (is.factor(overview[,v])) {
    barplot(table(overview[,v]), col=p_cols[grp], las=1, main=p_title, xlim=c(-.5, 3), ylim=c(0,500), border="white")
  } else {
    
    minmax <- range(dat, na.rm=T)
    
    n_breaks <- 40
    n_axis <- 6
    
    if (grepl("SOEP", v)) minmax <- c(0, 10)
    if (is.element(v, c("Dinv", "Dgam", "Dhea", "Drec", "Deth", "Dsoc"))) minmax <- c(1, 5)
    if (is.element(v, c("S.dfe2", "S.dfe4"))) {minmax <- c(0, 50); n_breaks <- 80; dat <- abs(dat)}
    if (v == "hhinc") minmax <- c(0, 8000)
    if (v == "R") {minmax <- c(-3, 3); n_axis=5}
    
    hist(dat, xlab="", col=p_cols[grp], breaks=n_breaks, xlim=minmax, las=1, border="white", xaxt="n", main=p_title)
    axis(1, at=seq(minmax[1], minmax[2], length.out=n_axis), cex=.7)
  }

}

dev.off()