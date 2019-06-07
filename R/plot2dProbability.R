plot2dProbability <- function(x1,x2,n=100,p=0.9,obs.only=FALSE,bandwith,...) {

    n <- length(x1)

    ## add some extra space
    x1lim <- range(x1)
    x1r <- x1lim[2]-x1lim[1]
    x1lim <- x1lim+c(-x1r/2,x1r/2)

    x2lim <- range(x2)
    x2r <- x2lim[2]-x2lim[1]
    x2lim <- x2lim+c(-x2r/2,x2r/2)

    ## kernel density
    if(missing(bandwith)) {
        kd <- kde2d(x=x1,y=x2,n=n,lims=c(x1lim,x2lim))
    } else {
        kd <- kde2d(x=x1,y=x2,n=n,h=bandwith,lims=c(x1lim,x2lim))    
    }

    ## grid spacing
    bs1 <- (x1lim[2]-x1lim[1])/(n-1)
    bs2 <- (x2lim[2]-x2lim[1])/(n-1)

    if(obs.only) {

        ## get densities at the location of the observations
        bg1 <- c(kd$x-bs1/2,kd$x[n]+bs1/2)
        bg2 <- c(kd$y-bs2/2,kd$y[n]+bs2/2)

        bc1 <- .bincode(x=x1,breaks=bg1)
        bc2 <- .bincode(x=x2,breaks=bg2)

        d <- kd$z[cbind(bc1,bc2)]

        ## grab densities which occur at p
        q <- quantile(d,probs=1-p)

        plot(x1,x2,xlim=x1lim,ylim=x2lim,...)
        contour(kd,levels=q,col=2,add=TRUE,labels=p,labcex=1)
        
    } else {

        ## integrate pdf, starting with the highest values
        d <- sort(kd$z,decreasing=TRUE)
        ps <- cumsum(d*bs1*bs2)

        ## grab densities which occur at p
        cl <- rep(NA,length(p))
        for(i in 1:length(p)) {cl[i] <- max(which(ps<=p[i]))}
        j <- is.finite(cl)

        plot(x1,x2,xlim=x1lim,ylim=x2lim,...)
        contour(kd,levels=d[cl[j]],col=2,add=TRUE,labels=p[j],labcex=1)

    }

    return(invisible())

}

