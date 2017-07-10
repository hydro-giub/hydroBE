plotHypsograph <- function(x,ylab='m a.s.l.',x.res=1e3,y.res=1e3,...) {

    ## coerce x to a vector
    dim(x) <- NULL

    ## sort elevation values
    ## NA values get removed
    z <- sort(x,na.last=NA)
    n <- length(z)

    ## calculate area in km2
    a.cell <- x.res*y.res*1e-6
    a.cum <- (n:1)*a.cell

    ## plot
    plot(x=a.cum,y=z,type='l',xlab=expression(km^2),ylab=ylab,...)
    return(invisible())
    
}
