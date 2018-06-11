### This script generates a latex table with the overview of the socio-demographic data
### (c) Renato Frey

load("../data/overview.Rdata")

library(xtable)

vars <- c("sex", "age", "empl", "hhinc", "eduyears")

tab <- c("N", "", "916")

for (var in vars) {

  vals <- overview[,var]
  
  if (is.numeric(vals)) {
    out <- paste("M = ", round(mean(vals, na.rm=T), 2),
                 " (SD = ", round(sd(vals, na.rm=T), 2),
                 ", Range = ", round(min(vals, na.rm=T), 2),
                 "--", round(max(vals, na.rm=T), 2),
                 ")", sep="")
    
    #out <- paste("M=", round(mean(vals, na.rm=T), 1), " (SD=", round(sd(vals, na.rm=T), 1), ")", sep="")
    
    curr_out <- cbind("", out)
    
  }
  
  if (is.factor(vals)) {
    
    if (var == "sex") levels(vals)[which(levels(vals) == 0)] <- "female"
    if (var == "sex") levels(vals)[which(levels(vals) == 1)] <- "male"
    
    out <- as.data.frame(table(vals))
    out_perc <- paste(round(prop.table(out[,2])*100, 1), "%", sep="")
    out[,2] <- paste(out[,2], " (", out_perc, ")", sep="")
  
    curr_out <- out
  }
  
  
  curr_out <- cbind(var, curr_out)
  if (nrow(curr_out) > 1) curr_out$var[2:nrow(curr_out)] <- ""
  
  tab <- rbind(tab, as.matrix(curr_out, colnames=NULL))
  if (var != tail(vars, 1)) tab <- rbind(tab, rep("", ncol(tab)))
  
}


labels_new <- tab[,1]
labels_new <- gsub("age", "Age", labels_new)
labels_new <- gsub("eduyears", "Education (years)", labels_new)
labels_new <- gsub("hhinc", "Household income", labels_new)
labels_new <- gsub("sex", "Sex", labels_new)
labels_new <- gsub("empl", "Employed", labels_new)
tab[,1] <- labels_new

xtab <- xtable(tab,
               type="latex",
               label=paste("tab:socdem"),
               caption=paste("Socio-demographic variables"),
               align=c("llll"),
               sanitize=identiy
               #align=c("lp{1in} ", rep("rp{1in} ", ncol(tab2)))
)

# . The values in brackets represent SDs and range. SES stands for ``socio-economic status''. To make the levels of education (``highest finished degree'') and income comparable across the two study centers, we created ordinal categories. Higher values indicate higher education or higher income.

output <- print(xtab,
                include.rownames=F,
                include.colnames=F,
                #table.placement = getOption("xtable.table.placement", "H"),
                caption.placement = "top",
                file="")

output <- sub("\\hline\n  \\hline\n", "\\hline\n", output, fixed=T)
output <- gsub("\\begin{table}", "\\begin{table*}", output, fixed=T)
output <- gsub("\\end{table}", "\\end{table*}", output, fixed=T)

cat(output)
cat(output, file="../output/tab_socdem.tex")