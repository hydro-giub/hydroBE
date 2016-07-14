estimateSnowFraction <- function(precip,temp,threshold=0.9,transition=1) {
  
  if (length (precip) != length (temp)) {
      stop('precip and temp have not the same length')
  }
  
  if(transition > 0) {
    f <- ifelse(temp > (threshold-transition) & temp < (threshold+transition),
                (threshold+transition-temp)/(2*transition),
                ifelse(temp < (threshold-transition),1,0))
  } else {
    f <- temp < threshold
  }
  
  return(f)
  
}
