applySnht <- function(y,x,method,omit=0) {

    snhtCriticalValues <- NULL
    load(file.path(path.package('hydroBE'),'extdata','snhtCriticalValues.rda'),
         envir=environment())

    n <- length(y)
    if(missing(x)) {
        q <- y
    } else {
        if(missing(method)) {stop('no method specified')}
        if(grepl('ratio',method)) {q <- y/x}
        if(grepl('diff',method)) {q <- y-x}
    }

    q <- scale(q)
    dim(q) <- NULL
    m <- matrix(NA,nrow=n-1,ncol=2)

    for(i in 1:(n-1)) {
        m[i,1] <- mean(q[1:i])^2
        m[i,2] <- mean(q[(i+1):n])^2
    }

    tv <- (1:(n-1))*m[,1]+((n-1):1)*m[,2]
    dim(tv) <- NULL
    i <- order(tv,decreasing=T)
    i <- setdiff(i,c(0:omit,(n-omit):n))

    j <- which.min(abs(snhtCriticalValues[,'n']-n))
    sig <- tv[i[1]]>snhtCriticalValues[j,-1]
    
    return(list(i=i[1],test=sig,t=tv,q=q))

}

