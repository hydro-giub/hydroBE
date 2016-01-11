getBFI <-
function(y=NULL,win=5) {
  
  n <- floor(length(y)/win)
  y <- y[1:(n*win)] 
  x <- rep(1:n,each=win)
  q <- tapply(y,x,function(x){ifelse(any(is.na(x)),NA,which.min(x))})+seq(from=0,by=5,length.out=n)
  
  i <- 2:(n-1)
  qq <- matrix(c(y[q[i-1]],y[q[i]],y[q[i+1]]),ncol=3)
  is.qb <- apply(qq,1,function(x){ifelse(any(is.na(x)),NA,x[2]<x[1]*0.9 & x[2]<x[3]*0.9)})
  is.qb <- c(NA,is.qb,NA)
  qb <- q[is.qb]
  
  x <- 1:length(y)
  yy <- approx(x=x[qb],y=y[qb],xout=x)$y
  t.gt <- yy > y
  t.gt[is.na(t.gt)] <- F
  yy[t.gt] <- y[t.gt]
  
  t.sub <- cbind(qb,c(qb[-1],qb[length(qb)]))
  t.in <- unique(unlist(apply(na.omit(t.sub),1,function(x){x[1]:x[2]})))
  sum(yy[t.in])/sum(y[t.in])
  
}
