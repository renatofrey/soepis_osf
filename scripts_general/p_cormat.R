### This script plots some correlation matrices
### (c) Renato Frey

library(polycor)
library(corrplot)

load("../data/overview.Rdata")
load("../data/cfa.Rdata")

dat_pred <- overview[,c("hhinc", "sex", "age", "fluid", "cryst", "eduyears", "empl", "social", "sports")]
colnames(dat_pred) <- labels[match(colnames(dat_pred), names(labels))]


colnames(dat_pred) <- gsub("Household income", "Household inc.", colnames(dat_pred), fixed=T)


pc_pred <- hetcor(dat_pred, use="pairwise.complete.obs")
cormat_pred <- pc_pred$correlations


pdf(file="../output/cormat_predictors.pdf")
corrplot(cormat_pred, method="color", type="lower", diag=T, is.cor=T, addCoef.col = "black", number.font=1, number.cex=.8, tl.col="red", cl.cex=1, cl.ratio=0.1, mar=c(0,0,0,5))
dev.off()
system("pdfcrop ../output/cormat_predictors.pdf ../output/cormat_predictors.pdf")



dat_dvs <- cbind(overview[,dvs_risk], R=pred[,"R"])

colnames(dat_dvs) <- gsub("SOEPgen", "General", colnames(dat_dvs), fixed=T)
colnames(dat_dvs) <- gsub("SOEPdri", "Driving", colnames(dat_dvs), fixed=T)
colnames(dat_dvs) <- gsub("SOEPinv", "Investment", colnames(dat_dvs), fixed=T)
colnames(dat_dvs) <- gsub("SOEPrec", "Recreation", colnames(dat_dvs), fixed=T)
colnames(dat_dvs) <- gsub("SOEPocc", "Occupation", colnames(dat_dvs), fixed=T)
colnames(dat_dvs) <- gsub("SOEPhea", "Health", colnames(dat_dvs), fixed=T)
colnames(dat_dvs) <- gsub("SOEPsoc", "Social", colnames(dat_dvs), fixed=T)

colnames(dat_dvs) <- gsub("S.dfe2", "Sample size (DFE2)", colnames(dat_dvs), fixed=T)
colnames(dat_dvs) <- gsub("S.dfe4", "Sample size (DFE4)", colnames(dat_dvs), fixed=T)
colnames(dat_dvs) <- gsub("R.dfe2", "Risky choice (DFE2)", colnames(dat_dvs), fixed=T)
colnames(dat_dvs) <- gsub("R.dfe4", "Risky choice (DFE4)", colnames(dat_dvs), fixed=T)
colnames(dat_dvs) <- gsub("R.dfd2", "Risky choice (DFD2)", colnames(dat_dvs), fixed=T)
colnames(dat_dvs) <- gsub("R.dfd4", "Risky choice (DFD4)", colnames(dat_dvs), fixed=T)
colnames(dat_dvs) <- gsub("\\bR\\b", "Psychom. model (R)", colnames(dat_dvs), perl=T)

pc_dvs <- hetcor(dat_dvs, use="pairwise.complete.obs")
cormat_dvs <- pc_dvs$correlations

pdf(file="../output/cormat_dvs.pdf", height=15)
corrplot(cormat_dvs, method="color", type="lower", diag=T, is.cor=T, addCoef.col = "black", number.font=1, number.cex=.7, tl.col="red", cl.cex=1, cl.ratio=0.1)
dev.off()
system("pdfcrop ../output/cormat_dvs.pdf ../output/cormat_dvs.pdf")