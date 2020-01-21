### This script runs the Bayesian regression analysis and is triggered by the seperate SLURM jobs
### (c) Renato Frey

# exemplary input parameters for this script (will be overwritten by actual arguments, if provided)
effect <- "age"
model <- 10
do_sim <- F
j_sim <- 1

# read arguments
args = (commandArgs(TRUE))
if (length(args) == 0) {
  print("No arguments supplied.")
} else {
  print(args)
  for (i in 1:length(args)) {
    eval(parse(text=args[[i]]))
  }
}

models <- read.csv(paste("~/Data/soep_is/sca/modlist_", effect, ".csv", sep=""), row.names=1)
for (var in 1:ncol(models)) {
  assign(colnames(models)[var], as.character(models[model,var]))
}

if (effect == "sex") sel_effect <- "sexfemale" else sel_effect <- effect

# define output directories
if (do_sim == F) dir_out <- paste("~/Data/soep_is/sca/", effect, sep="")
if (do_sim == T) dir_out <- paste("~/Data/soep_is/sca/sim/", effect, "/sim", formatC(j_sim, width=3, flag="0"), sep="")

library(reshape2)
library(rstanarm)
library(lme4)

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
colnames(long) <- gsub("value", "y", colnames(long))

# add dv_type
long$dv_type <- NA
long$dv_type[grepl("SOEP", long$variable)] <- "sr"
long$dv_type[is.element(long$variable, c("S.dfe2", "S.dfe4", "R.dfd2", "R.dfd4", "R.dfe2", "R.dfe4"))] <- "beh"

# assemble model formula
f <- "y ~"
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


## Pass 1: Estimate parameters

# models without random-effects
if (grepl("REM", dv) == F) {
  
  dat <- subset(long, variable == dv)
  
  # frequentist implementation
  mod_lm <- lm(formula = f, data = dat)
  s_lm <- summary(mod_lm)
  lm_est <- s_lm$coefficients[sel_effect, "Estimate"]
  lm_stderr <- s_lm$coefficients[sel_effect, "Std. Error"]
  lm_t <- s_lm$coefficients[sel_effect, "t value"]
  lm_p <- s_lm$coefficients[sel_effect, "Pr(>|t|)"]
  lm_r2 <- s_lm$r.squared
  
  # only run the Bayesian model when not simulating the null
  if (do_sim == F) {
  set.seed(8888)
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
  s_rstan <- summary(mod_rstan)
  rstan_est <- s_rstan[sel_effect, "mean"]
  rstan_HDIl <- s_rstan[sel_effect, "2.5%"]
  rstan_HDIu <- s_rstan[sel_effect, "97.5%"]
  rstan_r2 <- median(bayes_R2(mod_rstan))
  } else {
      mod_rstan <- NULL
      s_rstan <- NULL
      rstan_est <- NA
      rstan_HDIl <- NA
      rstan_HDIu <- NA
      rstan_r2 <- NA
  }
}

# models with random effects
if (grepl("REM", dv) == T) {
  
  dat <- long
  if (grepl("sr", dv) == T) dat <- subset(long, dv_type == "sr")
  if (grepl("beh", dv) == T) dat <- subset(long, dv_type == "beh")
  
  mod_lm <- lmer(formula = f, data = dat)
  s_lm <- summary(mod_lm)
  lm_est <- s_lm$coefficients[sel_effect, "Estimate"]
  lm_stderr <- s_lm$coefficients[sel_effect, "Std. Error"]
  lm_t <- NA
  lm_p <- NA
  lm_r2 <- NA
  
  # only run the Bayesian model when not simulating the null
  if (do_sim == F) {
    set.seed(8888)
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
    s_rstan <- summary(mod_rstan)
    rstan_est <- s_rstan[sel_effect, "mean"]
    rstan_HDIl <- s_rstan[sel_effect, "2.5%"]
    rstan_HDIu <- s_rstan[sel_effect, "97.5%"]
    rstan_r2 <- median(bayes_R2(mod_rstan))
  } else {
      mod_rstan <- NULL
      s_rstan <- NULL
      rstan_est <- NA
      rstan_HDIl <- NA
      rstan_HDIu <- NA
      rstan_r2 <- NA
  }
}



## Pass 2: Get distribution under the null
if (do_sim == T) {
  
  # create column with numeric values for sex
  dat$sex_num <- NA
  dat$sex_num[which(dat$sex == "male")] <- 0
  dat$sex_num[which(dat$sex == "female")] <- 1
  if (effect == "sex") sel_pred <- "sex_num" else sel_pred <- effect
  
  # get betas for actual effects
  b_lm <- s_lm$coefficients[sel_effect,"Estimate"]
  #b_rstan <- s_rstan[sel_effect,"mean"]
  
  # subtract effects from DV
  dat$ynull_lm <- dat[,"y"] - (dat[,sel_pred] * b_lm)
  #dat$ynull_rstan <- dat[,"y"] - (dat[,sel_pred] * b_rstan)
  
  # case-bootstrap (re-sampling)
  set.seed(j_sim)
  dat_boot <- dat[sample(1:nrow(dat), replace=T), ]
  
  # create new model formulas
  fnull_lm <- gsub("y ~", "ynull_lm ~", f, perl=T)
  #fnull_rstan <- gsub("y", "ynull_rstan", f)
  
  # run models
  if (grepl("REM", dv) == F) {
    
    mod_lm <- lm(formula = fnull_lm, data = dat_boot)
    s_lm <- summary(mod_lm)
    lm_est <- s_lm$coefficients[sel_effect, "Estimate"]
    lm_stderr <- s_lm$coefficients[sel_effect, "Std. Error"]
    lm_t <- s_lm$coefficients[sel_effect, "t value"]
    lm_p <- s_lm$coefficients[sel_effect, "Pr(>|t|)"]
    lm_r2 <- s_lm$r.squared
    
    # set.seed(j_sim)
    # mod_rstan <- stan_glm(
    #   formula = fnull_rstan,
    #   data = dat_boot,
    #   family = gaussian(), 
    #   prior_intercept = normal(0, 10),
    #   prior = normal(0, 2.5),
    #   prior_aux = cauchy(0, 5),
    #   cores = 3,
    #   chains = 3,
    #   iter = 2000)
    # s_rstan <- summary(mod_rstan)
    # r2_rstan <- bayes_R2(mod_rstan)
    mod_rstan <- NULL
    s_rstan <- NULL
    rstan_est <- NA
    rstan_HDIl <- NA
    rstan_HDIu <- NA
    rstan_r2 <- NA
  }
  
  if (grepl("REM", dv) == T) {
    
    mod_lm <- lmer(formula = fnull_lm, data = dat_boot)
    s_lm <- summary(mod_lm)
    lm_est <- s_lm$coefficients[sel_effect, "Estimate"]
    lm_stderr <- s_lm$coefficients[sel_effect, "Std. Error"]
    lm_t <- NA
    lm_p <- NA
    lm_r2 <- NA
    
    # set.seed(j_sim)
    # mod_rstan <- stan_glmer(
    #   formula = fnull,
    #   data = dat_boot,
    #   family = gaussian(), 
    #   prior_intercept = normal(0, 10),
    #   prior = normal(0, 2.5),
    #   prior_aux = cauchy(0, 5),
    #   cores = 3,
    #   chains = 3,
    #   iter = 2000)
    # s_rstan <- summary(mod_rstan)
    # r2_rstan <- bayes_R2(mod_rstan)
    mod_rstan <- NULL
    s_rstan <- NULL
    rstan_est <- NA
    rstan_HDIl <- NA
    rstan_HIDu <- NA
    rstan_r2 <- NA
  }
  
}


# collect results for lm
out <- data.frame(lm_est,
                  lm_stderr,
                  lm_t,
                  lm_p,
                  lm_r2,
                  rstan_est,
                  rstan_HDIl,
                  rstan_HDIu,
                  rstan_r2,
                  effect,
                  model,
                  do_sim,
                  j_sim,
                  models[model,],
                  timestamp=Sys.time(),
                  row.names=model
                  )


# do not save Rdata for simulation runs
if (do_sim == F) save(out, mod_lm, mod_rstan, file=paste(dir_out, "/", formatC(model, width=4, flag="0"), ".Rdata", sep=""))

write.csv(out, file=paste(dir_out, "/", formatC(model, width=4, flag="0"), ".csv", sep=""), row.names=F)