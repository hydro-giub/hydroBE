## test .getDuplicates()
library(hydroBE)
rm(list=ls())

x <- c('a','aa','b','a','a','d','b','b1')
d <- hydroBE:::.getDuplicates(x)

tt <- c(length(d)==length(x),
        all(is.null(unlist(d[c(2,4,5,6,7,8)]))),
        d[[1]]$to==1L,
        d[[1]]$from[1]==4L,
        d[[1]]$from[2]==5L,
        d[[3]]$to==3L,
        d[[3]]$from[1]==7L)

if(!all(tt)) {stop('.getDuplicates')}
