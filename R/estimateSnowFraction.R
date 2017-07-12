estimateSnowFraction <- function(y,thr=0.9,trans=1) {

    trans <- abs(trans)
    d <- dim(y)
    dim(y) <- NULL
    y[is.na(y)] <- 9999

    f <- rep(0,length(y))
    f[y<=(thr-trans)] <- 1

    if(trans>0) {
        i <- y<(thr+trans) & y>(thr-trans)
        f[i] <- (thr+trans-y[i])/(2*trans)
    }

    f[y>9990] <- NA
    dim(f) <- d
    return(f)
    
}
