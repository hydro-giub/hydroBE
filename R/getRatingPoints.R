getRatingPoints <- 
function(z,x,k,m,l,plot=FALSE) {
  
  # river bed reference point for water levels l
  cp <- which.min(z)
  # points to the left of cp
  lp <- 1:(min(cp)-1)
  # truncate to maximum
  lp <- lp[which.max(z[lp]):length(lp)]
  # points to the right of cp
  rp <- (max(cp+1)):length(z)
  # truncate to maximum
  rp <- rp[1:which.max(z[rp])]
  if (length(lp)<1 | length(rp)<1) {stop('please provide a meaningful profil z')}
  # truncate "overflow" water levels
  l <- l[(z[cp]+l<=max(z[lp])) & (z[cp]+l<=max(z[rp])) & l>0]
  if (length(l)<1) {stop('please provide suitable levels l')}
  k <- rep(k,length.out=length(z))
  # put the cleaned profil into a matrix
  pr <- matrix(c(x,z,k),ncol=3,dimnames=list(NULL,c('x','z','k')))[c(lp,cp,rp),]
  n <- nrow(pr)
  pq <- matrix(NA,nrow=length(l),ncol=6,dimnames=list(NULL,c('level','area','radius','k','velocity','discharge')))
  pq[,'level'] <- l
  
  for (i in 1:length(l)) {
    
    # initialise area
    a <- 0
    # initialise radius
    r <- 0
    # actual water level
    zl <- z[cp]+l[i]
    # is point wet? outermost points (min(lp) and max(rp) are always FALSE)
    iw <- pr[,'z']<zl
    # find most left and right points of segments
    sl <- which(xor(iw[2:(n-1)],iw[1:(n-2)]) & iw[2:(n-1)])+1
    sr <- which(xor(iw[2:(n-1)],iw[3:n]) & iw[2:(n-1)])+1
    
    for (j in 1:length(sl)) {
      
      # use intercept theorem to define end points: pay attention to signs because of overhanging segments!
      lz <- zl
      lx <- pr[sl[j],'x']+(pr[sl[j]-1,'x']-pr[sl[j],'x'])/(pr[sl[j]-1,'z']-pr[sl[j],'z'])*(lz-pr[sl[j],'z'])
      rz <- zl
      rx <- pr[sr[j],'x']+(pr[sr[j]+1,'x']-pr[sr[j],'x'])/(pr[sr[j]+1,'z']-pr[sr[j],'z'])*(rz-pr[sr[j],'z'])
      # calculate area of irregular poylgons according the Shoelace formula
      a <- a+1/2*abs(sum(c(lx,pr[sl[j]:sr[j],'x'],rx)*c(pr[sl[j]:sr[j],'z'],rz,lz)-c(pr[sl[j]:sr[j],'x'],rx,lx)*c(lz,pr[sl[j]:sr[j],'z'],rz)))
      # sum up radii
      d <- sqrt((c(pr[sl[j]:sr[j],'x'],rx)-c(lx,pr[sl[j]:sr[j],'x']))^2+(c(pr[sl[j]:sr[j],'z'],rz)-c(lz,pr[sl[j]:sr[j],'z']))^2)
      r <- r+sum(d)
      # weighted average of k according length of segments
      klr <- approx(x=pr[,'x'],y=pr[,'k'],xout=c(lx,rx))
      kd <- (c(klr$y[1],pr[sl[j]:sr[j],'k'])+c(pr[sl[j]:sr[j],'k'],klr$y[2]))/2
      pq[i,'k'] <- sum(kd*d)/sum(d)
      
    }
    
    pq[i,'area'] <- a
    pq[i,'radius'] <- r
    
  }
  
  pq[,'velocity'] <- sqrt(m)*(pq[,'area']/pq[,'radius'])^(2/3)*pq[,'k']
  pq[,'discharge'] <- pq[,'velocity']*pq[,'area'] 
  
  if(plot) {
    plot(x=x,y=z,type='n',ylab='z [m]',xlab='x [m]')
    polygon(x=c(lx,pr[sl[j]:sr[j],'x'],rx),y=c(lz,pr[sl[j]:sr[j],'z'],rz),col='deepskyblue',border=NA)
    abline(h=l,col='white',lwd=2,lty=3)
    lines(x=x,y=z)
    points(x=x[cp],y=z[cp],pch=20,cex=3,col=2)
    points(x=x,y=z,pch=20,cex=1.5)
    lines(x=rep(x[cp],2),y=c(z[cp],z[cp]+l[length(l)]))
    points(x=rep(x[cp],length(l)),y=z[cp]+l,pch='-',cex=2)
    text(x=rep(x[cp],length(l))+diff(par('usr')[1:2])/50,y=z[cp]+l+diff(par('usr')[3:4])/50,labels=paste(l,'m'),pos=4,offset=0)
  }
  
  return(pq)
  
}