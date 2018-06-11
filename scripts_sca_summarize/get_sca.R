### This script retrieves all the output data from the cluster analyses on sciCORE
### (c) Renato Frey

library(data.table)
get_sim <- T
n_spec <- 4352
n_sim <- 50

load("../data/overview.Rdata")
dvs <- c(dvs_risk, "R", "REM", "REMsr", "REMbeh")

effs <- list.dirs("../objects/scicore/sca", full.names=F, recursive=F)
effs <- effs[effs != "sim"]

effs <- effs[effs != "incomenet"]

missing_files <- NULL
write.csv(missing_files, "../data/sca/missing_files.csv", row.names=F)
minmax <- NULL
for (eff in effs) {
  
  # files <- list.files(paste("../objects/scicore/sca/", eff, sep=""), pattern="*.csv", full.names=T)
  files <- paste("../objects/scicore/sca/", eff, "/", formatC(1:n_spec, digits=3, flag=0), ".csv", sep="")

  results <- NULL
  sim_results <- NULL
  ctr <- 1
  results_file <- paste("../data/sca/results_", eff, ".csv", sep="")
  sim_results_file <- paste("../data/sca/sim_results_", eff, ".csv", sep="")
  write.csv(results, file=results_file, row.names=F)
  write.csv(sim_results, file=sim_results_file, row.names=F)
  
  # loop through all specifications
  for (file in files) {
    
    print(file)
    
    # save list of missing specifications
    if (!file.exists(file)) {
      missing_files <- rbind(missing_files, cbind(file, eff, dv=NA))
      next
    }
    
    curr_results <- fread(file)
    curr_dv <- gsub("'", "", curr_results$dv)
    
    df.dvs <- rbind(rep(0, length(dvs)))
    colnames(df.dvs) <- dvs
    df.dvs[which(colnames(df.dvs) == curr_dv)] <- 1
    
    curr_results <- cbind(as.data.frame(curr_results), df.dvs)
    
    # get simulation results for current specification
    curr_results$sim_median <- NA
    curr_results$sim_min <- NA
    curr_results$sim_max <- NA
    
    if (get_sim == T) {
      curr_sim <- NULL
      for (sim in 1:n_sim) {
        file_sim <- gsub(eff, paste("sim/", eff, "/sim", formatC(sim, width=3, flag="0"), sep=""), file)
        
        if (!file.exists(file_sim) | file.size(file_sim) == 0) {
          #missing_files <- rbind(missing_files, cbind(file_sim, eff, curr_dv))
        } else {
          curr_sim <- rbind(curr_sim, fread(file_sim))
        }
      }
      
      sim_results <- rbind(sim_results, curr_sim)
      
      # get timestamp of first / last simulation run
      if (!is.null(curr_sim)) {
        curr_range <- as.POSIXct(range(curr_sim$timestamp), origin = "1970-01-01", tz = "CET")
        if (is.null(minmax)) minmax <- curr_range else {
          if (curr_range[1] < minmax[1]) minmax[1] <- curr_range[1]
          if (curr_range[2] > minmax[2]) minmax[2] <- curr_range[2]
        }
      }
      
      # get quantiles of simulated effects for current specification (across all models)
      if (!is.null(curr_sim)) {
        curr_results$sim_median <- quantile(curr_sim$mean, .5)
        curr_results$sim_min <- quantile(curr_sim$mean, .025)
        curr_results$sim_max <- quantile(curr_sim$mean, .975)
      }
    }
    
    results <- rbind(results, curr_results)
    
    # save files temporarily to speed up processing
    ctr <- ctr + 1
    if (ctr == 500 | file == tail(files, 1)) {
      results_saved <- read.csv(results_file, blank.lines.skip=F)
      results <- rbind(results_saved, results)
      write.csv(results, file=results_file, row.names=F)
      
      if (get_sim == T) {
        sim_results_saved <- read.csv(sim_results_file, blank.lines.skip=F)
        sim_results <- rbind(sim_results_saved, as.data.frame(sim_results))
        write.csv(sim_results, file=sim_results_file, row.names=F)
      }
      
      missing_saved <- read.csv("../data/sca/missing_files.csv", blank.lines.skip=F)
      missing_files <- rbind(missing_files, missing_saved)
      write.csv(missing_files, "../data/sca/missing_files.csv", row.names=F)
      
      results <- NULL
      sim_results <- NULL
      missing_files <- NULL
      ctr <- 1
    }
  }
}