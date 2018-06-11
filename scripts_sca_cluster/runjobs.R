### This script creates the jobs on the cluster using the SLURM workload manager (see https://slurm.schedmd.com)
### (c) Renato Frey

load("../data/overview.Rdata")
load("../data/cfa.Rdata")

overview <- cbind(overview, pred)
overview_imp <- cbind(overview_imp, pred)

do_sim <- F
n_sim <- 50
prop_sim <- .25

opt <- c("no", "yes")
models_all <- expand.grid("dv" = c(dvs_risk, "R", "REM", "REMbeh", "REMsr"),
                          "sex" = opt,
                          "age" = opt,
                          "fluid" = opt,
                          "cryst" = opt,
                          "hhinc" = opt,
                          "eduyears" = opt,
                          "social" = opt,
                          "sports" = opt,
                          "empl" = opt)

# loop through all main predictors
for (effect in c("age", "sex", "fluid", "cryst", "hhinc", "eduyears")) {

  models <- subset(models_all, get(effect) == "yes")
  row.names(models) <- 1:nrow(models)
  write.csv(models, file=paste("~/Data/soep_is/sca/modlist_", effect, ".csv", sep=""))

  
  slicing <- c(rep(1, prop_sim * nrow(models)), rep(0, (1-prop_sim) * nrow(models)))
  slicing <- sample(slicing)
  
  # create job file
  jobs <- NULL
  
  for (j_sim in 1:n_sim) {
     
    if (do_sim == F & j_sim > 1) break
    
    print(paste("Run:", j_sim))

    # define output directories
    if (do_sim == F) dir_out <- paste("~/Data/soep_is/sca/", effect, sep="")
    if (do_sim == T) dir_out <- paste("~/Data/soep_is/sca/sim/", effect, "/sim", formatC(j_sim, width=3, flag="0"), sep="")
    
    # remove and (re)-create directories
    if (file.exists(dir_out) == T) system(paste("rm -r ", dir_out, sep=""))
    system(paste("mkdir -p ", dir_out, sep=""))
    
    for (j_model in 1:nrow(models)) {
      
      if (do_sim == T & slicing[j_model] == 0) next
    
      print(paste("Model", j_model, "/", nrow(models), "Models."))
      jobs <- rbind(jobs, paste("R CMD BATCH --no-save --no-restore --slave \"--args effect='", effect, "' model=", j_model, " do_sim=", do_sim, " j_sim=", j_sim, "\" models_fit.R ", dir_out, "/", formatC(j_model, width=4, flag="0"), "_Rout.txt", sep=""))
      
    }
  }
  
  write(jobs, file=paste("slurm/jobs_", effect, ".txt", sep=""))
  n_jobs <- nrow(jobs)
  
  job_string <- paste("#!/bin/bash 
#SBATCH --job-name=arrayjob_", effect, "
#SBATCH --cpus-per-task=3
#SBATCH --time=00:30:00
#SBATCH --qos=30min
#SBATCH --output=/dev/null
#SBATCH --error=/dev/null
#SBATCH --array=1-", n_jobs, "
module load R
module load JAGS
SEEDFILE=slurm/jobs_", effect, ".txt
SEED=$(sed -n ${SLURM_ARRAY_TASK_ID}p $SEEDFILE)
eval $SEED", sep="")
  
  write(job_string, file="slurm/slurm_script") 
  system("sbatch slurm/slurm_script")  
  
}