checkBafuFiles <- 
function(dir=NULL,files=NULL,check.series=FALSE,check.na=FALSE) {
  
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
    m[i1,c('name','site')] <- m[!i1,][i2,c('name','site')]
  }
  
  return(m)
  
}