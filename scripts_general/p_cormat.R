### This script plots some correlation matrices
### (c) Renato Frey

library(polycor)
library(corrplot)

load("../data/overview.Rdata")
load("../data/cfa.Rdata")

dat_pred <- overview[,c("sex", "age", "empl", "eduyears", "hhinc", "cryst", "fluid")]
colnames(dat_pred) <- labels[match(colnames(dat_pred), names(labels))]

pc_pred <- hetcor(dat_pred, use="pairwise.complete.obs")
cormat_pred <- pc_pred$correlations


pdf(file="../output/cormat_predictors.pdf")
corrplot(cormat_pred, method="color", type="lower", diag=F, is.cor=T, addCoef.col = "black", number.font=1, number.cex=.8, tl.col="red", cl.cex=1, cl.ratio=0.1)
dev.off()
system("pdfcrop ../output/cormat_predictors.pdf ../output/cormat_predictors.pdf")



dat_dvs <- cbind(overview[,dvs_risk], R=pred[,"R"])

pc_dvs <- hetcor(dat_dvs, use="pairwise.complete.obs")
cormat_dvs <- pc_dvs$correlations

pdf(file="../output/cormat_dvs.pdf", height=15)
corrplot(cormat_dvs, method="color", type="lower", diag=F, is.cor=T, addCoef.col = "black", number.font=1, number.cex=.7, tl.col="red", cl.cex=1, cl.ratio=0.1)
dev.off()
system("pdfcrop ../output/cormat_dvs.pdf ../output/cormat_dvs.pdf")