getReturnLevels <- function(f,per=c(30,100,300),alpha=0.05) {

    dn <- list(NULL,c('period','level','lower','upper'))
    rl <- matrix(NA,nrow=length(per),ncol=4,dimnames=dn)
    rl[,1] <- per
    mu <- f$mle[1]
    si <- f$mle[2]
    xi <- f$mle[3]
    p <- 1-1/per
    y <- -log(p)

    ## check if we have a Gumbel model
    if (abs(xi) > 0.001) {
        rl[,2] <- mu-si/xi*(1-y^(-xi))
    } else {
        rl[,2] <- mu-si*log(y)
    }

    pd <- c(rep(1,length(y)),
            -xi^(-1)*(1-y^(-xi)),
            si*xi^(-2)*(1-y^(-xi))-si*xi^(-1)*y^(-xi)*log(y))
    
    pd <- matrix(pd,ncol=3)
    v <- pd%*%f$cov%*%t(pd)
    rl[,3] <- rl[,2]-qnorm(1-alpha/2)*sqrt(diag(v))
    rl[,4] <- rl[,2]+qnorm(1-alpha/2)*sqrt(diag(v))

    return(rl)
    
}
