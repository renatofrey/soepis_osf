### This script generates a latex table of all risk-taking measures
### (c) Renato Frey

library(xtable)

load("../data/overview.Rdata")
dvs <- c(dvs_risk, "REM", "REMsr", "REMbeh")

dvs <- c("",
         dvs[grepl("SOEP", dvs)],
         "",
         dvs[!grepl("SOEP", dvs) & !grepl("REM", dvs)],
         "",
         c("R", dvs[grepl("REM", dvs)]))

table <- data.frame("Abbreviation" = dvs)
table$Measure <- c("\\textit{Propensity measures (self-reported)}",
                   "General risk preference",
                   "Driving",
                   "Investment",
                   "Recreational",
                   "Occupational",
                   "Health",
                   "Social",
                   "\\textit{Behavioral measures}",
                   "DFE (2 options): Sample size (inverted)",
                   "DFE (4 options): Sample size (inverted)",
                   "DFE (2 options): Proportion of risky choices",
                   "DFE (4 options): Proportion of risky choices",
                   "DFD (2 options): Proportion of risky choices",
                   "DFD (4 options): Proportion of risky choices",
                   "\\textit{Summary measures}",
                   "General factor (extracted from bifactor model)",
                   "Random-effects model (all measures)",
                   "Random-effects model (self-report measures)",
                   "Random-effects model (behavioral measures)")

table <- table[,c("Measure", "Abbreviation")]

capt <- "Measures of risk preference"
xtab <- xtable(table,
               caption = c(capt, ""),
               label = "tab:measures",
               type = "latex",
               align = paste(c("l", "l", rep("l", ncol(table)-1)), collapse=""))
               #align=c("lp{1in} ", rep("rp{1in} ", ncol(tab2)))

xtab2 <- print(xtab,
                include.rownames=F,
                include.colnames=T,
                sanitize.text.function = identity,
                table.placement = getOption("xtable.table.placement", "tb"),
                caption.placement = "top",
                file="")

# add table note
xtab2 <- gsub("\\end{tabular}", paste("\\multicolumn{", ncol(table), "}{l}{\\tiny Note. SOEP = Socioeconomic panel. DFE = Decisions from experience. DFD = Decisions from description.} \\\\ \\end{tabular}", sep=""), xtab2, fixed=T)

cat(xtab2, file=paste("../output/tab_measures.tex", sep=""))