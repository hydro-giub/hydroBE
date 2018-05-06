prewhitenSeries <- function(x,alpha=0.05,thr=0.001,max.iter=100) {

    n <- length(x)
    cl <- qnorm(1-alpha,sd=1/sqrt(n))

    xt1 <- numeric(n-1)
    xt2 <- numeric(n)
    
    a1 <- b1 <- Inf
    a2 <- acf(x,plot=FALSE)$acf[2,1,1]
    b2 <- sens.slope(x)$estimates
    ad <- bd <- Inf
    prw <- NA
    i <- 0
    
    if(a2 > cl) {

        while(ad > thr | bd > thr) {            
            
            i <- i+1

            xt1[] <- (x[-1]-a2*x[-n])/(1-a2)
            b2 <- sens.slope(xt1)$estimates
            xt2[] <- x-b2*(1:n)
            a2 <- acf(xt2,plot=FALSE)$acf[2,1,1]
            
            if(!(a2 > cl)) {break}
            
            ad <- abs(a1-a2)
            bd <- abs(b1-b2)
            
            a1 <- a2
            b1 <- b2
            
            if(i>=max.iter) {
                warning(paste('still no convergence after',
                              i,'iterations'))
                break
            }
            
        }

        xt1 <- c(NA,xt1)
        prw <- TRUE
        
    } else {

        xt1 <- x
        a1 <- a2
        prw <- FALSE
        
    }

    attributes(b2) <- NULL
    
    return(list(x=xt1,prewhitend=prw,acoef=a1,slope=b2,iter=i))

}
