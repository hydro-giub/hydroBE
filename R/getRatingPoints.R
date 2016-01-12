getRatingPoints <- 
function(z,x,m,k,baseRef=NULL,l,ni=1000,plot=FALSE) {
  
  n <- length(z)
  z <- z*(-1)
  if(is.null(baseRef)) {baseRef <- which.max(z)}
  b <- mean(z[baseRef])
  l <- l[l<(max(z)-b)]
  k <- rep(k,length.out=n)
  
  # resampling according x (for area)
  xx1 <- seq(from=x[1],x[n],length.out=ni) 
  dx1 <- xx1[2]-xx1[1]
  zz1 <- approx(x=x,y=z,xout=xx1,ties=min)$y
  
  # resampling according distance (for radius and k)
  dx2 <- c(0,sqrt((x[2:n]-x[1:(n-1)])^2+(z[2:n]-z[1:(n-1)])^2))
  xx2 <- seq(from=dx2[1],to=sum(dx2),length.out=ni)
  zz2 <- approx(x=cumsum(dx2),y=z,xout=xx2)$y
  dr <- xx2[2]-xx2[1]
  kk <- approx(x=cumsum(dx2),y=k,xout=xx2)$y
  
  pq <- matrix(NA,nrow=length(l),ncol=6,dimnames=list(NULL,c('level','area','radius','k','velocity','discharge')))
  pq[,'level'] <- l
  for (i in 1:length(l)) {
    pp <- b+l[i]
    ix <- zz1<pp
    pq[i,'area'] <- sum((pp-zz1[ix])*dx1)
    ix <- zz2<pp
    pq[i,'radius'] <- sum(ix)*dr
    pq[i,'k'] <- mean(kk[ix])
  }
  pq[,'velocity'] <- sqrt(m)*(pq[,'area']/pq[,'radius'])^(2/3)*pq[,'k']
  pq[,'discharge'] <- pq[,'velocity']*pq[,'area'] 
  
  if(plot) {
    plot(x=x,y=z,type='n',ylab='z [m]',xlab='x [m]')
    abline(h=b,lty=3,col=2,lwd=1.5)
    polygon(x=x,y=z,col='deepskyblue',border=NA)
    for (i in 1:length(l)) {
      pp <- b+l[i]
      ix <- zz1<pp
      lines(x=xx1[range(which(ix))],y=rep(pp,2),col='white',lwd=2,lty=3)
    }
    lines(x=x,y=z)
    points(x=x[baseRef],y=z[baseRef],pch=20,cex=3,col=2)
    points(x=x,y=z,pch=20,cex=1.5)
    lines(x=rep(x[min(baseRef)],2),y=c(b,0))
    points(x=rep(x[min(baseRef)],length(l)),y=b+l,pch='-',cex=2)
    text(x=rep(x[min(baseRef)],length(l))+diff(par('usr')[1:2])/50,y=b+l+diff(par('usr')[3:4])/50,labels=paste(l,'m'),pos=4,offset=0)
  }
  
  return(pq)
  
}