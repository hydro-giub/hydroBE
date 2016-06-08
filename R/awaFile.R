checkAwaFiles <- 
function(dir=NULL,files=NULL,check.series=FALSE,check.na=FALSE) {
  
  if(!is.null(dir)) {files <- list.files(dir,full.names=T,pattern='\\.(asc|txt|csv)$')}
  cn <- c('id','name','site','var','res','type','start','end','nas','comment',
          'file','format','hydropro','sep','skip','nlines')
  m <- matrix(NA,nrow=length(files),ncol=length(cn),dimnames=list(NULL,cn))
  
  for(i in 1:length(files)) {
    
    fil <- files[i]
    frt <- sub('^.*?\\.(.*$)','\\1',fil)
    
    co <- file(fil,encoding='CP1282')
    li <- readLines(con=co,n=30)
    close(co)
    nlines <- floor(file.info(fil)$size/(nchar(li[30])+2))
    
    hyp <- any(grepl('[hH]ydro|[hH][bB][bB][eE]',li))
    
    if(hyp) {
      h <- awaHeaderHydropro(li=li)
      m[i,names(h)] <- unlist(h)
      if(check.series) {
        h <- awaSeriesHydropro(file=fil,nlines=nlines,skip=h$skip,check.na=check.na,series.only=F)
        m[i,names(h)] <- unlist(h)
      }
    }
    
    m[i,'file'] <- fil
    m[i,'format'] <- frt
    m[i,'hydropro'] <- hyp
    m[i,'nlines'] <- nlines
    
  }
  
  i1 <- is.na(m[,'name']) & is.na(m[,'site'])
  if(any(i1)) {
    i2 <- match(m[i1,'id'],sub('^[[:digit:][:blank:]/]*?([[:digit:]]+)$','\\1',m[!i1,'id']))
    m[i1,c('name','site')] <- m[!i1,][i2,c('name','site')]
  }
  
  return(m)
  
}

importAwaFiles <- 
function(dir=NULL,files=NULL,ignore.var=FALSE,simplify.time=FALSE,quiet=TRUE) {
  
  if(!is.null(dir)) {files <- list.files(dir,full.names=T)}
  fs <- checkAwaFiles(files=files,check.series=T,check.na=F)
  id <- fs[,'id']
  e <- grepl('^$',id)
  if(any(e)) {id[e] <- 1:sum(e)}
  if(simplify.time) {fs[,'res'] <- sub('irregular|unknown','10 min',fs[,'res'])}
  tr <- unique(fs[,'res'])
  if(length(tr)!=1) {warning('temporal resolution is not consistent')}
  tr <- setdiff(tr,c('irregular','unknown'))[1]
  if(is.na(tr)) {return(F)}
  
  d <- getTimestamps(res=tr,sm=fs)
  x <- d$x
  ig <- simplify.time & grepl('[[:digit:]]{2}:[[:digit:]]{2}$',x[1])
  if(ig) {x <- sub('[[:digit:]]{1}$','',x)}
  m1 <- matrix(NA,nrow=length(x),ncol=nrow(fs),dimnames=list(as.character(x),paste0('id',id)))
  hyp <- as.logical(fs[,'hydropro'])
  
  for(i in 1:nrow(fs)) {
    if(hyp[i]) {
      m2 <- awaSeriesHydropro(file=fs[i,'file'],nlines=fs[i,'nlines'],skip=fs[i,'skip'],check.na=F,series.only=T)
    }
    m1[substr(rownames(m2),1,ifelse(ig,d$n-1,d$n)),i] <- m2[,'y']
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