testLinearTrend <- function(x,n=1000,ret.slopes=F) {
    
    nn <- length(x)
    xx <- 1:nn
    s0 <- coef(lm(x~xx,data=data.frame(x=x,xx=xx),model=F,x=F,y=F,qr=F))[2]
    sn <- rep(NA,n)
    
    for(i in 1:n) {
        sn[i] <- coef(lm(x~xx,data=data.frame(x=x[sample.int(n=nn)],xx=xx),
                         model=F,x=F,y=F,qr=F))[2]
    }
    
    sn <- sort(sn)
    r <- findInterval(s0,sn)
    p <- min(r/n,1-r/n)
    
    l <- list(b=unname(s0),p.value=p)
    if(ret.slopes) {l[['slopes']] <- sn}
    
    return(l)

}
