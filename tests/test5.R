w <- 5
x <- rnorm(100)
o1 <- .Call(hydroBE:::runSum,x,as.integer(w))
o2 <- filter(x,filter=rep(1,times=w),method='convolution',sides=1)

tt <- is.na(o1[1:4])
if(!all(tt)) {stop('getRunSum -- NA values')}

tt <- length(o1)==length(o2)
if(!all(tt)) {stop('getRunSum -- vector length')}

tt <- round(o1[-(1:4)],5)==round(o2[-(1:4)],5)
if(!all(tt)) {stop('getRunSum -- filter')}
