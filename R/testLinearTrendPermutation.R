testLinearTrendPermutation <- function(x,nper=1000,per.slopes=F) {
  
  n <- length(x)
  xx <- 1:n
  s0 <- coef(lm(x~xx,data=data.frame(x=x,xx=xx),model=F,x=F,y=F,qr=F))[2]
  sn <- rep(NA,nper)
  
  for(i in 1:nper) {
    sn[i] <- coef(lm(x~xx,data=data.frame(x=x[sample.int(n=n)],xx=xx),model=F,x=F,y=F,qr=F))[2]
  }
  
  sn <- sort(sn)
  r <- findInterval(s0,sn)
  p <- min(r/nper,1-r/nper)
  
  l <- list(slope=unname(s0),p.value=p)
  if(per.slopes) {l[['per.slopes']] <- sn}
  
  return(l)

}