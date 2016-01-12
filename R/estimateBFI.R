estimateBFI <-
function(y=NULL,win=5,r=0.9,plot=FALSE) {
  
  # get local minimas in blocks of size win
  n <- length(y)
  y <- y[1:(n-n%%win)]
  n <- length(y)/win
  x <- rep(1:n,each=win)
  q <- tapply(y,x,function(x){ifelse(any(is.na(x)),NA,which.min(x))})+seq(from=0,by=5,length.out=n)
  
  # get baseflow ordinates
  i <- 2:(n-1)
  qq <- matrix(c(y[q[i-1]],y[q[i]],y[q[i+1]]),ncol=3)
  is.qb <- apply(qq,1,function(x){ifelse(any(is.na(x)),NA,x[2]*r<x[1] & x[2]*r<x[3])})
  is.qb <- c(NA,is.qb,NA)
  qb <- q[is.qb]
  
  # linear approximation
  x <- 1:length(y)
  yy <- approx(x=x[qb],y=y[qb],xout=x)$y
  gt <- yy > y
  gt[is.na(gt)] <- F
  yy[gt] <- y[gt]
  
  # define subperiods with respect to NAs
  sp <- cbind(qb,c(qb[-1],qb[length(qb)]))
  ti <- unique(unlist(apply(na.omit(sp),1,function(x){x[1]:x[2]})))
  xx <- 1:length(y)
  xx[-ti] <- NA
  
  # calculate ratio
  bfi <- sum(y[ti])/sum(yy[ti])
  
  # plot
  if(plot) {
    plot(y,type='l')
    points(x=x[q],y=y[q],pch=20)
    points(x=x[qb],y=y[qb],col=2)
    lines(x=x[xx],y=yy[xx],col=2)
  }
  
  return(bfi)
  
}
