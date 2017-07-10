testBreakpoint <- function(x,alpha=0.05) {
    
    n <- length(x)
    m <- matrix(NA,nrow=n,ncol=3,dimnames=list(NULL,c('rank','cumsum','ut')))
    
    ## test statistic u
    m[,'rank'] <- rank(x)
    m[,'cumsum'] <- cumsum(m[,'rank'])
    m[,'ut'] <- 2*m[,'cumsum']-(1:n)*(n+1)
    m[n,'ut'] <- 0
    
    ## breakpoint k at index i
    i <- which.max(abs(m[,'ut']))
    k <- unname(abs(m[i,'ut'])) 
    
    ## P(K>k)
    p <- 2*exp((-6*(k^2))/(n^2+n^3))
    
    ## k for which P(K>k) = alpha
    l <- sqrt(-1/6*log(alpha/2,base=exp(1))*(n^2+n^3))
    
    return(list(i=i,p.value=round(p,3),k=k,critical.value=round(l,3),reject.H0=k>=l,u=m[,'ut']))
    
}
