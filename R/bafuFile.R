checkBafuFiles <- function(dir=NULL,files=NULL,check.series=FALSE,check.na=FALSE) {
  
  if(!is.null(dir)) {files <- list.files(dir,full.names=T,pattern='\\.(asc|txt|csv|xls)$')}
  cn <- c('id','name','site','var','res','type','start','end','nas','comment',
          'file','format','hydropro','sep','skip','nlines')
  m <- matrix(NA,nrow=length(files),ncol=length(cn),dimnames=list(NULL,cn))
  
  for(i in 1:length(files)) {
    
    fil <- files[i]
    frt <- sub('^.*?\\.(.*$)','\\1',fil)
    xls <- grepl('xls',frt)
    
    if(xls) {
      li <- gdata::read.xls(fil,fileEncoding='ISO-8859-1',quote='"',nrows=30,header=F,sep='x',stringsAsFactors=F)[,1]
      nlines <- -1L
    } else {
      co <- file(fil,encoding='ISO-8859-1')
      li <- readLines(con=co,n=30)
      close(co)
      nlines <- floor(file.info(fil)$size/(nchar(li[30])+2))
    }
    
    hyp <- any(grepl('[hH]ydro|[hH][bB][cC][hH]',li))
    
    if(!xls & !hyp) {
      h <- bafuHeaderTxt(li=li)
      m[i,names(h)] <- unlist(h)
      if(check.series) {
        h <- bafuSeriesTxt(file=fil,nlines=nlines,skip=h$skip,check.na=check.na,series.only=F)
        m[i,names(h)] <- unlist(h)
      }
    }
    
    if(!xls & hyp) {
      h <- bafuHeaderHydropro(li=li)
      m[i,names(h)] <- unlist(h)
      if(check.series) {
        h <- bafuSeriesHydropro(file=fil,nlines=nlines,skip=h$skip,check.na=check.na,series.only=F)
        m[i,names(h)] <- unlist(h)
      }
    }
    
    if(xls & !hyp) {
      h <- bafuHeaderXls(li=li)
      m[i,names(h)] <- unlist(h)
      if(check.series) {
        h <- bafuSeriesXls(file=fil,nlines=nlines,skip=h$skip,check.na=check.na,series.only=F)
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
    m[i1,c('name','site')] <- m[!i1,,drop=F][i2,c('name','site')]
  }
  
  return(m)
  
}

importBafuFiles <- function(dir=NULL,files=NULL,ignore.var=FALSE,quiet=TRUE) {

  if(!is.null(dir)) {files <- list.files(dir,full.names=T)}
  fs <- checkBafuFiles(files=files,check.series=T,check.na=F)
  id <- as.integer(sub('^.*?/[[:blank:]]*','',fs[,'id']))
  e <- grepl('^$',id)
  if(any(e)) {id[e] <- 1:sum(e)}
  tr <- unique(fs[,'res'])
  if(length(tr)!=1) {warning('temporal resolution is not consistent')}
  tr <- setdiff(tr,c('irregular','unknown'))[1]
  if(is.na(tr)) {return(F)}
  
  d <- getTimestamps(res=tr,sm=fs)
  x <- d$x
  m1 <- matrix(NA,nrow=length(x),ncol=nrow(fs),dimnames=list(as.character(x),paste0('id',id)))
  hyp <- as.logical(fs[,'hydropro'])
  xls <- grepl('xls',fs[,'format'],fixed=T)
  
  for(i in 1:nrow(fs)) {
    if(!xls[i] & !hyp[i]) {
      m2 <- bafuSeriesTxt(file=fs[i,'file'],nlines=fs[i,'nlines'],skip=fs[i,'skip'],check.na=F,series.only=T)
    }
    if(!xls[i] & hyp[i]) {
      m2 <- bafuSeriesHydropro(file=fs[i,'file'],nlines=fs[i,'nlines'],skip=fs[i,'skip'],check.na=F,series.only=T)
    }
    if(xls[i] & !hyp[i]) {
      m2 <- bafuSeriesXls(file=fs[i,'file'],nlines=fs[i,'nlines'],skip=fs[i,'skip'],check.na=F,series.only=T)
    }
    m1[substr(rownames(m2),1,d$n),i] <- m2[,'y']
  }
  
  cn <- colnames(m1)
  ci <- 1:nrow(fs)
  i <- 2
  while(i<=ncol(m1)) {
    ii <- which(cn[i]==cn[1:(i-1)])
    if(!ignore.var) {ii <- ii[fs[ci[ii],'var']==fs[ci[i],'var'] & fs[ci[ii],'type']==fs[ci[i],'type']]}
    if(length(ii)==1) {
      rpl <- is.na(m1[,ii]) & !is.na(m1[,i])
      m1[rpl,ii] <- m1[rpl,i]
      m1 <- m1[,-i,drop=F]
      cn <- cn[-i]
      ci <- ci[-i]
    } else {i <- i+1}
  }
  
  colnames(m1) <- paste(cn,fs[ci,'type'],fs[ci,'var'],sep='.')
  if(!quiet) {print(fs)}
  return(m1)
  
}
