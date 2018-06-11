### This script plots the bifactor model
### (c) Renato Frey

library(shape)
library(lavaan)

# define some colors
p_cols <- cbind(beh="#80FFFFFF",
                freq="#BFFF80FF",
                prop="#FF8080FF",
                F="orange")

# use colors that should also be distinguishable for color-blind people?
if (T) {
  library(viridis)
  p_cols[1:3] <- viridis(3, begin=0.5, end=1, alpha=1)[]
  p_cols[4] <- "grey30"
}

p_cols[,"F"] <- gray(.3)

#quartz()
par(mar=c(7,1,2,1))
if (vers == "3trad_bifred") par(mar=c(8,3,4,3))
par(mar=c(8,3,6,3))

pars <- parameterEstimates(fit)

var_r <- data.frame(subset(pars, lhs == "R" & op == "=~")[,c("lhs", "rhs", "est")])
var_e <- data.frame(subset(pars, lhs == rhs & est != 1)[,c("lhs", "rhs", "est")])
var_f <- data.frame(subset(pars, is.element(lhs, paste("F", 1:10, sep="")) & op == "=~")[,c("lhs", "rhs", "est")])

# fix if there is not R-factor
if (nrow(var_r) == 0) {
  bifact_present <- F
  var_r <- var_e
  var_r$lhs <- "R"
  var_r$est <- 0
} else bifact_present <- T



print(paste("% Explained:", 1-round(sum(var_e$est)/nrow(var_r), 3)))
print(paste("% Unexplained:", round(sum(var_e$est)/nrow(var_r), 3)))

print(paste("% Total (r):", round(sum(var_r[,"est"]^2) / nrow(var_r), 3)))
print(paste("% Total (factors):", round(sum(var_f[,"est"]^2) / nrow(var_r), 3)))
print(paste("% Total (r + factors):", round((sum(var_r[,"est"]^2) + sum(var_f[,"est"]^2)) / nrow(var_r), 3)))

print(paste("% Explained (r):", round(sum(var_r[,"est"]) / (sum(var_r[,"est"]) + sum(var_f[,"est"])), 3)))
print(paste("% Explained (factors):", round(sum(var_f[,"est"]) / (sum(var_r[,"est"]) + sum(var_f[,"est"])), 3)))


#tmp <- subset(pars, is.element(lhs, paste("F", 1:10, sep="")) & op == "=~")[,c("rhs", "est")]
#var_f <- as.data.frame(tapply(tmp$est, list(tmp$rhs), sum))
var_f2 <- 1 - var_e[,"est"] - var_r[match(var_e$rhs, var_r$rhs),"est"]^2

var <- data.frame(var_r=var_r$est^2,
                  var_e=var_e[match(var_r$rhs, var_e$rhs),"est"],
                  var_f=var_f2)

row.names(var) <- var_r$rhs
var <- t(var)

var[is.na(var)] <- 0


var <- cbind(var[,which(!grepl("SOEP", colnames(var)))],
             var[,which(grepl("SOEP", colnames(var)))])


cols <- c(adjustcolor(as.character(p_cols[,"freq"]), .3), "white", as.character(p_cols[,"freq"]))
if (T) {
  ind_prop <- which(is.element(colnames(var), grps$prop))
  ind_beh <- which(is.element(colnames(var), grps$beh))
  ind_freq <- which(is.element(colnames(var), grps$freq))
  var <- rbind(var, var, var)
  var[c(1:3,7:9), ind_prop] <- 0
  var[4:9, ind_beh] <- 0
  var[1:6, ind_freq] <- 0
  
  cols <- c(adjustcolor(as.character(p_cols[,"beh"]), .3), "transparent", as.character(p_cols[,"beh"]),
            adjustcolor(as.character(p_cols[,"prop"]), .3), "transparent", adjustcolor(as.character(p_cols[,"prop"])),
            adjustcolor(as.character(p_cols[,"freq"]), .3), "transparent", as.character(p_cols[,"freq"]))
}

#var <- var[,order(match(colnames(var), dvs_risk))]


if (F) {
  var[3,1:9] <- 0
  var[2,1:9] <- 1-var[1,1:9]
  
  var[6,10:25] <- 0
  var[5,10:25] <- 1-var[4,10:25]
}


scale <- 6
scale_u <- scale*1.7
scale_l <- scale*-0.6
b <- barplot((var*scale), axes=F, ylim=c(scale_l, scale_u), las=2, col=cols, asp=1, xpd=T) #38.2
n <- length(b)


#abline(v=b, col="lightgrey")

#r2 <- .75
#floating.pie(xpos=3, ypos=5, radius=1, x=c(r2,1-r2), col=c("orange", "white"), lty=1)

# Specific Factors
Fy <- 5 + scale/1.25
rad <- 1.5

if (T) {
  Fy <- 4 + scale/1.25
  rad <- 1
}

Fpaths <- subset(pars, is.element(lhs, paste("F", 1:10, sep="")) & op == "=~")
Fpaths$xpos <- match(Fpaths$rhs, colnames(var))
Fs <- unique(Fpaths$lhs)
MANs <- unique(Fpaths$rhs)

Fx <- seq(min(b), max(b), length.out=length(Fs))
if (vers == "3trad_bifred") Fx <- seq(min(b)-3, max(b)+3, length.out=length(Fs))

#x_ord <- 1:length(Fx)
x_ord <- rank(tapply(Fpaths$xpos, list(Fpaths$lhs), mean), ties.method="first")
#x_ord <- c(3,1,2,4)

symbols(cbind(Fx,Fy), circles=rep(rad, length(Fx)), inches=F, add=T, fg="white", bg=p_cols[,"F"], asp=1)
text(cbind(Fx[x_ord],Fy), Fs, cex=1.2, col="white")

for (i in 1:nrow(Fpaths)) {
  p1 <- as.numeric(substr(Fpaths$lhs[i], 2, 99))
  p2 <- match(Fpaths$rhs[i], colnames(var))
  
  #Fx_new <- Fx[x_ord]
  
  if (Fpaths$est[i] >= 0) {
    p_col <- "black"
    p_lty = 1
  }
  else {
    p_col <- "red"
    p_lty <- 3
  }
  
  Arrows(Fx[x_ord][p1], Fy-rad, b[p2], scale, arr.type="triangle", arr.adj=1, arr.length=.15, arr.width=.1, lty=p_lty)  
  
}

for (i in 1:length(b)) {
  if (is.element(colnames(var)[i], grps$prop)) curr_col <- as.character(p_cols[,"prop"])
  if (is.element(colnames(var)[i], grps$beh)) curr_col <- as.character(p_cols[,"beh"])
  if (is.element(colnames(var)[i], grps$freq)) curr_col <- as.character(p_cols[,"freq"])
  lines(x=c(b[i],b[i]), y=c(-14,0), col=curr_col)
}


# Bifactor
if (bifact_present == T) {
  Rx <- mean(b)
  Ry <- scale_l - 1
  if (T) Ry <- -2.5
  symbols(cbind(Rx,Ry), circles=rad, inches=F, add=T, fg="white", bg=p_cols[,"F"])
  text(cbind(Rx,Ry), expression(bolditalic("R")), cex=1.2, col="white")
  
  
  for (i in 1) {
    for (j in 1:n) {
      if (var_r$est[j] >= 0) {
        p_col <- "black"
        p_lty = 1
      }
      else {
        p_col <- "red"
        p_lty <- 3
      }
      Arrows(Rx, Ry+(rad), b[j], 0, arr.type="triangle", arr.adj=1, arr.length=.15, arr.width=.1, lty=p_lty)  
    }
  }
}

title(p_fits)