declusterExtremes <- function(x,L=11) {
    
    n <- length(x)
    d <- 1:n
    
    if(n>2) {
        i <- which(x[-c(1,n)]>x[-(1:2)] & x[-c(1,n)]>x[-c(n-1,n)])+1
        ii <- which.max(x[i])
        i <- i[ii]
        t1 <- rep(F,n)
        t1[d<(i-L+1)] <- TRUE
        t2 <- rep(F,n)
        t2[d>(i+L-1)] <- TRUE
        ti1 <- declusterExtremes(x=x[t1],L=L)
        ti2 <- declusterExtremes(x=x[t2],L=L)
        ti <- c(ti1,i,ti2+sum(!t2))
    } else {
        ti <- numeric(0)
    }

    return(ti)
    
}
