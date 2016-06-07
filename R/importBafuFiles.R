importBafuFiles <- 
function(dir=NULL,files=NULL,ignore.var=FALSE,quiet=TRUE) {

  if(!is.null(dir)) {files <- list.files(dir,full.names=T)}
  fs <- checkBafuFiles(files=files,check.series=T,check.na=F)
  id <- as.integer(sub('^.*?/[[:blank:]]*','',fs[,'id']))
  e <- !is.finite(id)
  if(any(e)) {id[e] <- 1:sum(e)}
  tr <- unique(fs[,'res'])
  if(length(tr)!=1) {warning('temporal resolution is not consistent')}
  tr <- setdiff(tr,c('irregular','unknown'))[1]
  if(is.na(tr)) {return(F)}
  
  if(tr=='year') {
    r <- range(as.numeric(sub('\\..*$','',fs[,'start'])),as.numeric(sub('\\..*$','',fs[,'end'])))
    x <- seq.int(from=r[1],to=r[2],by=1)
    nt <- 4
  }
  if(tr=='month') {
    r <- range(as.numeric(sub('\\..*$','',fs[,'start'])),as.numeric(sub('\\..*$','',fs[,'end'])))
    i <- rep(seq(from=r[1],to=r[2],by=1),each=12)
    i <- paste(i,formatC(rep(1:12,times=length(i)/12),flag='0',width=2),sep='.')
    r <- range(match(c(fs[,'start'],fs[,'end']),i))
    x <- i[r[1]:r[2]]
    nt <- 7
  }
  if(tr=='day') {
    r <- range(as.POSIXct(as.vector(fs[,c('start','end')]),format='%Y.%m.%d',tz='UTC'))
    x <- format(seq.POSIXt(from=r[1],to=r[2],by='day',tz='UTC'),'%Y.%m.%d')
    nt <- 10
  }
  if(tr=='hour') {
    r <- range(as.POSIXct(as.vector(fs[,c('start','end')]),format='%Y.%m.%d %H',tz='UTC'))
    x <- format(seq.POSIXt(from=r[1],to=r[2],by='hour',tz='UTC'),'%Y.%m.%d %H')
    nt <- 13
  }
  if(tr=='10 min') {
    r <- range(as.POSIXct(as.vector(fs[,c('start','end')]),format='%Y.%m.%d %H:%M',tz='UTC'))
    x <- format(seq.POSIXt(from=r[1],to=r[2],by='10 min',tz='UTC'),'%Y.%m.%d %H:%M')
    nt <- 16
  }
  
  m1 <- matrix(NA,nrow=length(x),ncol=nrow(fs),dimnames=list(as.character(x),paste0('id',id)))
  hyp <- as.logical(fs[,'hydropro'])
  xls <- grepl('xls',fs[,'format'],fixed=T)
  
  for (i in 1:nrow(fs)) {
    if(!xls[i] & !hyp[i]) {
      m2 <- bafuSeriesTxt(file=fs[i,'file'],nlines=fs[i,'nlines'],skip=fs[i,'skip'],check.na=F,series.only=T)
    }
    if(!xls[i] & hyp[i]) {
      m2 <- bafuSeriesHydropro(file=fs[i,'file'],nlines=fs[i,'nlines'],skip=fs[i,'skip'],check.na=F,series.only=T)
    }
    if(xls[i] & !hyp[i]) {
      m2 <- bafuSeriesXls(file=fs[i,'file'],nlines=fs[i,'nlines'],skip=fs[i,'skip'],check.na=F,series.only=T)
    }
    m1[substr(rownames(m2),1,nt),i] <- m2[,'y']
  }
  
  cn <- colnames(m1)
  ci <- 1:nrow(fs)
  i <- 2
  while (i<=ncol(m1)) {
    ii <- which(cn[i]==cn[1:(i-1)])
    if(!ignore.var) {ii <- ii[fs[ci[ii],'var']==fs[ci[i],'var'] & fs[ci[ii],'type']==fs[ci[i],'type']]}
    if(length(ii)==1) {
      rpl <- is.na(m1[,ii]) & !is.na(m1[,i])
      m1[rpl,ii] <- m1[rpl,i]
      m1 <- m1[,-i]
      cn <- cn[-i]
      ci <- ci[-i]
    } else {i <- i+1}
  }
  
  colnames(m1) <- paste(cn,fs[ci,'type'],fs[ci,'var'],sep='.')
  if(!quiet) {print(fs)}
  return(m1)
  
}