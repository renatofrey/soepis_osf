### This script runs the Bayesian regression analysis and is triggered by the seperate SLURM jobs
### (c) Renato Frey

# exemplary input parameters for this script
# effect <- "sex"
# model <- 3
# do_sim <- F
# j_sim <- 2

# read arguments
args = (commandArgs(TRUE))
print(args)
if (length(args) == 0) {
  print("No arguments supplied.")
} else {
  for (i in 1:length(args)) {
    eval(parse(text=args[[i]]))
  }
}

models <- read.csv(paste("~/Data/soep_is/sca/modlist_", effect, ".csv", sep=""), row.names=1)
for (var in 1:ncol(models)) {
  assign(colnames(models)[var], as.character(models[model,var]))
}

# define output directories
if (do_sim == F) dir_out <- paste("~/Data/soep_is/sca/", effect, sep="")
if (do_sim == T) dir_out <- paste("~/Data/soep_is/sca/sim/", effect, "/sim", formatC(j_sim, width=3, flag="0"), sep="")

if (do_sim == F) set.seed(88888)

library(reshape2)
library(rstanarm)

load("../data/overview.Rdata")
load("../data/cfa.Rdata")

overview <- cbind(overview, pred)
overview_imp <- cbind(overview_imp, pred)

dvs <- c(dvs_risk, "R")

# transform variables?
if (T) {
  for (dv_trans in c(dvs, "age", "fluid", "cryst", "incomegro", "hhinc", "incomenet", "eduyears", "social", "sports")) {
    # center
    overview[,dv_trans]  <- overview[,dv_trans] - mean(overview[,dv_trans], na.rm=T)
    
    # rescale (standardize)
    overview[,dv_trans] <- overview[,dv_trans] / sd(overview[,dv_trans], na.rm=T)
  }
}

# add quadratic age term (equivalen to using poly(age, 2, raw=T)
overview$age2 <- overview$age^2

# transform data into long format
df.dvs <- overview[,dvs]
df.dvs$partid <- overview$partid
long <- melt(df.dvs, id.vars="partid")
long <- long[order(long$partid),]
long <- cbind(long, overview[match(long$partid, overview$partid), ])

# add dv_type
long$dv_type <- NA
long$dv_type[grepl("SOEP", long$variable)] <- "sr"
long$dv_type[is.element(long$variable, c("S.dfe2", "S.dfe4", "R.dfd2", "R.dfd4", "R.dfe2", "R.dfe4"))] <- "beh"

# assemble model formula
f <- "value ~"
try(if (sex == "yes") f <- paste(f, " + sex", sep=""), silent=T)
try(if (age == "yes") f <- paste(f, " + age", sep=""), silent=T)
try(if (agesqr == "yes") f <- paste(f, " + age2", sep=""), silent=T)
try(if (fluid == "yes") f <- paste(f, " + fluid", sep=""), silent=T)
try(if (cryst == "yes") f <- paste(f, " + cryst", sep=""), silent=T)
try(if (famstat == "yes") f <- paste(f, " + famstat", sep=""), silent=T)
try(if (incomegro == "yes") f <- paste(f, " + incomegro", sep=""), silent=T)
try(if (incomenet == "yes") f <- paste(f, " + incomenet", sep=""), silent=T)
try(if (hhinc == "yes") f <- paste(f, " + hhinc", sep=""), silent=T)
try(if (eduyears == "yes") f <- paste(f, " + eduyears", sep=""), silent=T)
try(if (sports == "yes") f <- paste(f, " + sports", sep=""), silent=T)
try(if (social == "yes") f <- paste(f, " + social", sep=""), silent=T)
try(if (empl == "yes") f <- paste(f, " + empl", sep=""), silent=T)
try(if (grepl("REM", dv) == T) f <- paste(f, " + (1 | partid)", sep=""), silent=T)
f <- gsub("~ +", "~", f, fixed=T)

# select correct data and run model
if (grepl("REM", dv) == F) {
  
  dat <- subset(long, variable == dv)
  
  # if simulation mode, shuffle main predictor variable
  if (do_sim == T) dat[,effect] <- sample(dat[,effect])

  mod_rstan <- stan_glm(
    formula = f,
    data = dat,
    family = gaussian(), 
    prior_intercept = normal(0, 10),
    prior = normal(0, 2.5),
    prior_aux = cauchy(0, 5),
    cores = 3,
    chains = 3,
    iter = 2000)
}

if (grepl("REM", dv) == T) {
  
  dat <- long
  if (grepl("sr", dv) == T) dat <- subset(long, dv_type == "sr")
  if (grepl("beh", dv) == T) dat <- subset(long, dv_type == "beh")
  
  # if simulation mode, shuffle main predictor variable
  if (do_sim == T) dat[,effect] <- sample(dat[,effect])
  
  mod_rstan <- stan_glmer(
    formula = f,
    data = dat,
    family = gaussian(), 
    prior_intercept = normal(0, 10),
    prior = normal(0, 2.5),
    prior_aux = cauchy(0, 5),
    cores = 3,
    chains = 3,
    iter = 2000)
}

r2 <- bayes_R2(mod_rstan)

args_m <- as.data.frame(matrix(unlist(strsplit(args, split="=")), nrow=2))
colnames(args_m) <- as.character(unlist(args_m[1,]))
args_m <- args_m[-1,]

if (effect == "sex") sel_effect <- "sexfemale" else sel_effect <- effect

out <- c(summary(mod_rstan)[sel_effect,c("mean", "2.5%", "97.5%")], r2_med=median(r2), r2_sd=sd(r2), timestamp=Sys.time())
out <- as.data.frame(t(out))
names(out) <- gsub("2.5%", "HDIl", names(out))
names(out) <- gsub("97.5%", "HDIu", names(out))

out <- cbind(out, models[model,], effect=effect, model=model, do_sim=do_sim, j_sim=j_sim, row.names=1)

# do not save Rdata for simulation runs
if (do_sim == F) save(out, mod_rstan, file=paste(dir_out, "/", formatC(model, width=4, flag="0"), ".Rdata", sep=""))

write.csv(out, file=paste(dir_out, "/", formatC(model, width=4, flag="0"), ".csv", sep=""), row.names=F)
