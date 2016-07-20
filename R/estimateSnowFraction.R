estimateSnowFraction <- function(temp,thr=0.9,trans=1) {
  
  temp <- as.array(temp)
  d <- dim(temp)
  dim(temp) <- prod(d)
  
  if(trans > 0) {
    f <- ifelse(temp > (thr-trans) & temp < (thr+trans),
                (thr+trans-temp)/(2*trans),
                ifelse(temp < (thr-trans),1,0))
  } else {
    f <- temp < thr
  }
  
  dim(f) <- d
  return(f)
  
}