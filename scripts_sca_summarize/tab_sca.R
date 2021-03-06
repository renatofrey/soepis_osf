### This script generates the latex tables with the SCA results
### (c) Renato Frey

library(data.table)
library(xtable)

load("../data/overview.Rdata")
dvs <- c(dvs_risk, "REM", "REMsr", "REMbeh")

p_effs <- list.dirs("../objects/scicore/sca", full.names=F, recursive=F)
p_effs <- c("hhinc", "sex", "age", "fluid", "cryst", "eduyears")

table <- NULL

get_signif <- function(est, se) {
  z <- est / se
  p <- exp(-0.717 * z - 0.416 * z^2)
  sign <- rep(NA, length(est))
  sign[which(p < 0.05)] <- 1
  sign[which(p >= 0.05)] <- 0
  return(sign)
}

SDs <- NULL
infs <- NULL

for (p_eff in p_effs) {
  
  print(p_eff)
  
  results <- fread(paste("../data/sca/results_", p_eff, ".csv", sep=""))
  sim_results <- fread(paste("../data/sca/sim_results_", p_eff, ".csv", sep=""))
  n_sims <- table(sim_results$model)
  n_sim <- max(n_sims)
  
  print(paste("Number of specifications:", dim(results)[1]))
  print(table(n_sims))
  
  # stats of actual results
  results$sign <- sign(results$rstan_est)
  results$signif <- get_signif(results$lm_est, results$lm_stderr)
  results$cred <- (results$rstan_HDIl < 0 & results$rstan_HDIu < 0) | (results$rstan_HDIl > 0 & results$rstan_HDIu > 0)
    
  eff_median <- median(results$rstan_est)
  eff_median_pos <- median(results$rstan_est[results$rstan_est > 0])
  eff_median_neg <- median(results$rstan_est[results$rstan_est < 0])

  cred_n <- sum(results$cred)
  cred_prop <- prop.table(table((results$cred)))["TRUE"]
  cred_pos_n <- tapply(results$cred, list(results$sign), sum)[2]
  cred_neg_n <- tapply(results$cred, list(results$sign), sum)[1]
  cred_pos_prop <- cred_pos_n / nrow(results)
  cred_neg_prop <- cred_neg_n / nrow(results)

  signif_n <- sum(results$signif)
  signif_prop <- prop.table(table(results$signif))["1"]
  
  
  ifns <- rbind(infs, data.frame("cred" = table(results$cred)["TRUE"],
                                 "sign" = table(results$signif)["1"])
  )
  
  
  # stats of simulations
  if (!is.null(sim_results$rstan_est)) {
  sim_results$sign <- sign(sim_results$lm_est)
  sim_results$signif <- get_signif(sim_results$lm_est, sim_results$lm_stderr)
  sim_results$cred <- (sim_results$rstan_HDIl < 0 & sim_results$rstan_HDIu < 0) | (sim_results$rstan_HDIl > 0 & sim_results$rstan_HDIu > 0)
  
  sim_cred_n <- tapply(sim_results$cred, sim_results$j_sim, sum)
  sim_cred_prop <- sim_cred_n / tapply(sim_results$cred, sim_results$j_sim, length)
  
  sim_signif_n <- tapply(sim_results$signif, sim_results$j_sim, sum)
  sim_signif_prop <- sim_signif_n / tapply(sim_results$signif, sim_results$j_sim, length)
  
  sim_diff <- which(sim_signif_prop >= signif_prop)
  if (length(sim_diff) == 0) sim_diff <- 0
  sim_p <- (length(sim_diff) / n_sim) / 2
  if (sim_p < .001) sim_p <- "<.001" else sim_p <- formatC(sim_p, digits=2, format="f")
  } else {
    sim_cred_prop <- NA
    cred_pos_pro <- NULL
    cred_neg_pro <- NULL
  }
  
  p_eff_lab <- p_eff
  if (p_eff == "fluid") p_eff_lab <- "Fluid intelligence"
  if (p_eff == "cryst") p_eff_lab <- "Cryst. intelligence"
  if (p_eff == "eduyears") p_eff_lab <- "Years of education"
  if (p_eff == "hhinc") p_eff_lab <- "Household income"
  if (p_eff == "incomenet") p_eff_lab <- "personal income"
  if (p_eff == "sex") p_eff_lab <- "Sex (female)"
  p_eff_lab <- sub("^.", toupper(substr(p_eff_lab, 1, 1)), p_eff_lab)
  
  table <- rbind(table, data.frame(c1 = p_eff_lab,
                                   c2 = nrow(results),
                                   c3 = paste(round(eff_median, 2), sep=""),
                                   c4 = paste(cred_pos_n, " (", formatC(round(cred_pos_prop, 3)*100, digits=1, format="f"), "%)", sep=""),
                                   c5 = paste(cred_neg_n, " (", formatC(round(cred_neg_prop, 3)*100, digits=1, format="f"), "%)", sep=""),
                                   tmp = max(sim_cred_prop),
                                   c6 = paste(as.character(length(sim_diff)), "/", n_sim),
                                   c7 = sim_p))
  
  # " (",
  # round(eff_median_pos, 2), " / ",
  # round(eff_median_neg, 2),
  # ")", 
  
  SDs <- c(SDs, sd(sim_signif_prop))
  print("********************")
  
}

print(SDs)
print(mean(SDs))

rownames(infs) <- p_effs
print(infs)
print(cor(infs))

#print(table)
table <- table[,-which(colnames(table) == "tmp")]

labels <- c("Candidate driver",
            "Number of specifications",
            "Median effect size across all specifications",
            "Specifications with credible positive effects",
            "Specifications with credible negative effects",
            "Number of shuffled samples with a larger proportion of credible effects than for the original sample",
            "Exact p-value of permutation test")
colnames(table) <- labels


for (k in 1:2) {
  
  if (k == 1) p_table <- table[]

  if (k == 2) p_table <- table[!is.element(table$`Candidate driver`,
                                          c("Sex (female)",
                                            "Age",
                                            "Fluid intelligence",
                                            "Income")),]
  
  if (k == 1) {
    t_lab <- "tab:sca_results"
    t_cap <- c("Results of the specification curve analyses", "")
    t_placement <- getOption("xtable.table.placement", "t!")
  }
  if (k == 2) {
    t_lab <- "tab:sca_results_si"
    t_cap <- c("Results of additional specification curve analyses")
    t_placement <- getOption("xtable.table.placement", "ht")
  }


xtab <- xtable(p_table,
               caption = t_cap,
               label = t_lab,
               type = "latex",
               align = paste(c("l", "l", rep("r", ncol(p_table)-1)), collapse=""))
               #align=c("lp{1in} ", rep("rp{1in} ", ncol(tab2)))

xtab2 <- print(xtab,
                include.rownames=F,
                include.colnames=T,
                table.placement = t_placement,
                caption.placement = "top",
                file="",
                only.contents = TRUE)


repl <- " & Number of & Median posterior effect size & Number (prop.) & Number (prop.) & Bootstrap samples with & Exact p-value of \\\\\\\\
 & specifications & across all specifications & of credible & of credible & larger proportion of & bootstrap test \\\\\\\\
 &  & & positive effects & negative effects & significant effects & "
# (only pos. / neg. effects)

xtab2 <- gsub(paste(labels, collapse=" & "), repl, xtab2)

# make two-columns
#### xtab2 <- gsub("table", "table*", xtab2)


if (F) {
# convert into threeparttable
xtab2 <- gsub("\\begin{table}", "\\begin{threeparttable}", xtab2, fixed=T)
xtab2 <- gsub("\\end{table}", "\\end{threeparttable}", xtab2, fixed=T)
}

xtab2 <- gsub("\\end{tabular}", "  \\end{tabular} \\begin{tablenotes}
      \\footnotesize Note. Candidate correlates are sorted by median effect size across all specifications.
    \\end{tablenotes}", xtab2, fixed=T)


xtab2 <- gsub("\\label{tab:sca_results}", "\\label{tab:sca_results} \\resizebox{\\textwidth}{!}{%", xtab2, fixed=T)

xtab2 <- gsub("end{tabular}", "end{tabular}}", xtab2, fixed=T)



if (k == 1) cat(xtab2, file=paste("../output/tab_sca.tex", sep=""))
if (k == 2) cat(xtab2, file=paste("../output/tab_sca_si.tex", sep=""))

}