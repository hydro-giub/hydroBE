estimateBreakpoint <- function(x, alpha) {
  
  # rank and rank_cumsum
  xtmp <- cbind(x, rank(x[,2]))
  xtmp <- cbind(xtmp, cumsum(xtmp[,3]))
  xtmp <- cbind(xtmp, 1:nrow(xtmp))
  
  # Xk (test statistic)
  xtmp <- cbind(xtmp, 2 * xtmp[,4] - xtmp[,5] * (nrow(xtmp) + 1))
  colnames(xtmp) <- c('YYYY', 'VALUE', 'rank', 'rank_cumsum', 'k', 'Xk')
  
  # breakpoint
  XE <- max(abs(xtmp[,'Xk']))
  XEa <- xtmp[which(abs(xtmp[,'Xk'])==XE), 'YYYY']
  
  # p-value
  n <- nrow(xtmp)
  pval <- exp( (-6 * (XE^2)) / (n^2 + n^3) )
  
  # significance level (global for all series with same n)
  Xksign <- sqrt( -1/6 * ((log(alpha, base = exp(1))) * (n^2 + n^3)) )
  significant <- XE > Xksign
  
  res <- list(Xk=xtmp,XEa=XEa,XE=XE,Xksign=Xksign,sig=significant,pval=pval)
  return(res)
}