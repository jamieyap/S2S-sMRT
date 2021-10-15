source("paths.R")
load(file.path(path_staged_data, "allroots_ee_missdat.RData"))
load(file.path(path_staged_data, "allroots_ee_rr.RData"))

head(allroots_ee_missdat, n=20)

exp(allroots_ee_missdat[,"PSI0"])
exp(allroots_ee_missdat[,"PSI0"] + allroots_ee_missdat[,"ETA0"])

# Average bias in the estimates
mean(log(0.70) - allroots_ee_missdat[,"PSI0"])
mean(0 - allroots_ee_missdat[,"ETA0"])

# Variance of estimates
var(allroots_ee_missdat[,"PSI0"])
var(allroots_ee_missdat[,"ETA0"])
var(allroots_ee_rr[,"ALPHA1"])
var(allroots_ee_rr[,"ALPHA2"])
var(allroots_ee_rr[,"BETA11"])
var(allroots_ee_rr[,"BETA10"])
var(allroots_ee_rr[,"BETA21"])
var(allroots_ee_rr[,"BETA20"])

# What do the dots in allroots_ee_rr represent?
head(allroots_ee_rr, n=20)

# Let's see if they are identical to zero or are simply numerically equivalent to zero
tmp <- allroots_ee_rr[-11,]
var(tmp[,"ALPHA1"])
var(tmp[,"ALPHA2"])
var(tmp[,"BETA11"])
var(tmp[,"BETA10"])
var(tmp[,"BETA21"])
var(tmp[,"BETA20"])

