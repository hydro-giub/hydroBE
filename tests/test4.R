## test testBreakpoint()
library(hydroBE)
library(trend)
rm(list=ls())

x <- rnorm(100)
t1 <- testBreakpoint(x)
t2 <- pettitt.test(x)

tt <- c(t1$i==t2$estimate,
        round(t1$p.value,3)==round(t2$p.value,3),
        t1$k==t2$statistic)

if(!all(tt)) {stop('testBreakpoint')}
