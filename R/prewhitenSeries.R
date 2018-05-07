prewhitenSeries <- function(x,alpha=0.05,thr=0.001,max.iter=100) {

    n <- length(x)
    cl <- qnorm(1-alpha,sd=1/sqrt(n))

    xt1 <- numeric(n-1)
    xt2 <- numeric(n)
    
    a1 <- acf(x,plot=FALSE)$acf[2,1,1]
    b1 <- sens.slope(x)$estimates
    a2 <- b2 <- Inf
    ad <- bd <- Inf
    prw <- FALSE
    i <- 0

    if(a1>cl) {

        prw <- TRUE
        xt1[] <- (x[-1]-a1*x[-n])/(1-a1)
        
        while((ad>thr | bd>thr) & i<max.iter) {            

            i <- i+1

            b2 <- sens.slope(xt1)$estimates
            bd <- abs(b1-b2)
            b1 <- b2

            xt2[] <- x-b1*(1:n)
            a2 <- acf(xt2,plot=FALSE)$acf[2,1,1]

            if(a2<cl) {break}

            ad <- abs(a1-a2)
            a1 <- a2

            xt1[] <- (x[-1]-a1*x[-n])/(1-a1)
            
        }

        x[] <- c(NA,xt1)
        
    }
    
    if(i==max.iter) {
        warning(paste('still no convergence after',
                      i,'iterations'))
    }
    
    attributes(b1) <- NULL
    
    return(list(x=x,prewhitend=prw,acoef=a1,slope=b1,iter=i))

}
