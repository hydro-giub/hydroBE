getReturnLevelsGev <- function(f=NULL,rp=NULL,a=NULL) {
  
  rl <- matrix(NA,nrow=length(rp),ncol=4,dimnames=list(NULL,c('returnP','returnL','deltaL','deltaU')))
  rl[,1] <- rp
  mu <- f$mle[1]
  si <- f$mle[2]
  xi <- f$mle[3]
  p <- 1-1/rp
  y <- -log(p)
  if (abs(xi) > 0.001) {rl[,2] <- mu-si/xi*(1-y^(-xi))} else {rl[,2] <- mu-si*log(y)}
  pd <- matrix(c(rep(1,length(y)),-xi^(-1)*(1-y^(-xi)),si*xi^(-2)*(1-y^(-xi))-si*xi^(-1)*y^(-xi)*log(y)),ncol=3)
  v <- pd%*%f$cov%*%t(pd)
  rl[,3] <- rl[,2]-qnorm(1-a/2)*sqrt(diag(v))
  rl[,4] <- rl[,2]+qnorm(1-a/2)*sqrt(diag(v))
  return(rl)
  
}
