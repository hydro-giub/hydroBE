getCrps <- function(cdfx,cdfy,obs) {

    nx <- length(cdfx)
    swl <- cdfx[2L]-cdfx[1L]
    swr <- cdfx[nx]-cdfx[nx-1L]
    cdfx <- c(cdfx[1]-swl,cdfx,cdfx[nx]+swr)
    cdfy <- c(0,cdfy,1)
    nx <- nx+2L
    
    if(obs<cdfx[1]) {
        F1 <- cdfx[1]-obs
        f2 <- approxfun(x=cdfx,y=(1-cdfy)^2,yleft=1,yright=0)
        F2 <- integrate(f2,lower=cdfx[1],upper=cdfx[nx],
                        stop.on.error=FALSE)
        if(F2$message!='OK') {print(F2$message)}
        F2 <- F2$value
    }

    if(obs>cdfx[nx]) {            
        F1 <- obs-cdfx[nx]
        f2 <- approxfun(x=cdfx,y=cdfy^2,yleft=0,yright=1)
        F2 <- integrate(f2,lower=cdfx[1],upper=cdfx[nx],
                        stop.on.error=FALSE)
        if(F2$message!='OK') {print(F2$message)}
        F2 <- F2$value
    }

    if(obs>=cdfx[1] && obs<=cdfx[nx]) {
        f1 <- approxfun(x=cdfx,y=cdfy^2,yleft=0,yright=1)
        F1 <- integrate(f1,lower=cdfx[1],upper=obs,
                        stop.on.error=FALSE)
        if(F1$message!='OK') {print(F1$message)}
        F1 <- F1$value
        f2 <- approxfun(x=cdfx,y=(1-cdfy)^2,yleft=1,yright=0)
        F2 <- integrate(f2,lower=obs,upper=cdfx[nx],
                        stop.on.error=FALSE)
        if(F2$message!='OK') {print(F2$message)}
        F2 <- F2$value
    }

    return(F1+F2)

}
