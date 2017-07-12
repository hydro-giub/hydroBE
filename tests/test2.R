## test .getDuplicates() function
library(hydroBE)
rm(list=ls())

x <- c('a','aa','b','a','a','d','b','b1')
d <- hydroBE:::.getDuplicates(x)

if(length(d)!=length(x)) {stop()}

for(i in c(2,4,5,6,7,8)) {if(!is.null(d[[i]])) {stop()}}

if(d[[1]]$to!=1L) {stop()}
if(d[[1]]$from[1]!=4L) {stop()}
if(d[[1]]$from[2]!=5L) {stop()}

if(d[[3]]$to!=3L) {stop()}
if(d[[3]]$from[1]!=7L) {stop()}
