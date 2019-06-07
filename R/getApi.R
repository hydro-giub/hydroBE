getApi <- function(x,k=0.9,n=5,finite=TRUE) {

    l <- length(x)
    y <- rep(NA,times=l)

    if(finite) {

        if(length(k)==1) {
            kn <- rep(NA,times=n)
            for(i in 1:n) kn[i] <- k^(n-i)
        } else {
            n <- length(k)
            kn <- sort(k)
        }
        
        for(i in (n+1):l) {
            y[i] <- t(kn)%*%x[(i-n):(i-1)]
        }
        
    } else {

        k <- max(k)
        y[2] <- x[1]
        for(i in 3:l) {y[i] <- k*y[i-1]+x[i-1]}
        
    }
    
    return(y)

}
