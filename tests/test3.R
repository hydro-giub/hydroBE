## test getCrps()
library(hydroBE)
rm(list=ls())

## unif(0,1)
cdfx <- seq(0,1,by=0.001)
cdfy <- cdfx

## x < obs
F1 <- function(x) 1/3*x^3

## x >= obs
F2 <- function(x) x - x^2 + 1/3*x^3

## obs = -1
crps1 <- getCrps(cdfx=cdfx,cdfy=cdfy,obs=-1)
crps2 <- 1 + (F2(1) - F2(0))
if(round(crps1,5)!=round(crps2,5)) {stop('getCrps')}

## obs = 2
crps1 <- getCrps(cdfx=cdfx,cdfy=cdfy,obs=2)
crps2 <- (F1(1) - F1(0)) + 1
if(round(crps1,5)!=round(crps2,5)) {stop('getCrps')}

## obs = 0.5
crps1 <- getCrps(cdfx=cdfx,cdfy=cdfy,obs=0.5)
crps2 <- (F1(0.5) - F1(0)) + (F2(1) - F2(0.5)) 
if(round(crps1,5)!=round(crps2,5)) {stop('getCrps')}

## obs = 0.2
crps1 <- getCrps(cdfx=cdfx,cdfy=cdfy,obs=0.2)
crps2 <- (F1(0.2) - F1(0)) + (F2(1) - F2(0.2)) 
if(round(crps1,5)!=round(crps2,5)) {stop('getCrps')}

## obs = 0.8
crps1 <- getCrps(cdfx=cdfx,cdfy=cdfy,obs=0.8)
crps2 <- (F1(0.8) - F1(0)) + (F2(1) - F2(0.8)) 
if(round(crps1,5)!=round(crps2,5)) {stop('getCrps')}

