declusterExtremes <- function(y=NULL,L=NULL) {
  
  n <- length(y)
  d <- 1:n
  if(n>2) {
    i <- which(y[-c(1,n)]>y[-(1:2)] & y[-c(1,n)]>y[-c(n-1,n)])+1
    ii <- which.max(y[i])
    i <- i[ii]
    t1 <- rep(F,n)
    t1[d<(i-L+1)] <- T
    t2 <- rep(F,n)
    t2[d>(i+L-1)] <- T
    ti1 <- declusterExtremes(y=y[t1],L=L)
    ti2 <- declusterExtremes(y=y[t2],L=L)
    ti <- c(ti1,i,ti2+sum(!t2))
  } else {ti <- numeric(0)}
  return(ti)
  
}
