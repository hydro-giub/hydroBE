bafuSeries <-
function(li=NULL,check.na=FALSE,sblock=NULL) {
  
  n <- na2 <- length(li)
  if(is.null(sblock)) {sblock <- n}
  na1 <- sum(grepl('NA',li,fixed=T))
  li <- sub(';.*$','',li)
  d <- as.POSIXct(li,format='%Y.%m.%d %H:%M',tz='UTC')
  
  h <- list(res=NA,start=NA,end=NA,nas=NA)
  h[c('start','end')] <- format(d[c(1,n)],'%Y.%m.%d %H:%M')
  h$res <- ifelse(n>1,'irregular','unknown')
  
  dd <- diff(as.numeric(d[1:sblock]))
  if(sum(dd==10*60,na.rm=T)>(sblock*0.8)) {
    h$res <- '10 min'
    if(check.na) {na2 <- as.numeric(difftime(d[n],d[1],units='mins'))/10+1}
  }
  
  d <- as.POSIXct(sub(':[[:digit:]]{2}$',':00',li),format='%Y.%m.%d %H:%M',tz='UTC')
  dd <- diff(as.numeric(d[1:sblock]))
  if(sum(dd==60*60,na.rm=T)>(sblock*0.8)) {
    h$res <- 'hour'
    h[c('start','end')] <- format(d[c(1,n)],'%Y.%m.%d %H')
    if(check.na) {na2 <- as.numeric(difftime(d[n],d[1],units='hours'))+1}
  }
  
  d <- as.POSIXct(sub(' [[:digit:]:]+$','',li),format='%Y.%m.%d',tz='UTC')
  dd <- diff(as.numeric(d[1:sblock]))
  if(sum(dd==24*60*60,na.rm=T)>(sblock*0.8)) {
    h$res <- 'day'
    h[c('start','end')] <- format(d[c(1,n)],'%Y.%m.%d')
    if(check.na) {na2 <- as.numeric(difftime(d[n],d[1],units='days'))+1}
  }
  
  d <- as.POSIXct(sub('\\.[[:digit:]]{2} [[:digit:]:]+$','.01',li),format='%Y.%m.%d',tz='UTC')
  dd <- diff(as.numeric(d[1:sblock]))
  if(sum(dd>26*24*60*60 & dd<33*24*60*60,na.rm=T)>(sblock*0.8)) {
    h$res <- 'month'
    h[c('start','end')] <- format(d[c(1,n)],'%Y.%m')
    if(check.na) {
      na2 <- (13-as.integer(format(d[1],'%m')))+as.integer(format(d[n],'%m'))+
        (as.integer(format(d[n],'%Y'))-as.integer(format(d[1],'%Y'))-1)*12
    }
  }
  
  d <- as.POSIXct(sub('\\.[[:digit:]]{2}\\.[[:digit:]]{2} [[:digit:]:]+$','.01.01',li),format='%Y.%m.%d',tz='UTC')
  dd <- diff(as.numeric(d[1:sblock]))
  if(sum(dd>363*24*60*60 & dd<368*24*60*60,na.rm=T)>(sblock*0.8)) {
    h$res <- 'year'
    h[c('start','end')] <- format(d[c(1,n)],'%Y')
    if(check.na) {na2 <- as.integer(h$end)-as.integer(h$start)+1}
  }
  
  if(check.na & !grepl('irregular',h$res)) {h$nas <- na1+na2-n}
  return(h)
  
}

bafuSeriesTxt <-
function(file=NULL,nlines=0,skip=3,check.na=FALSE,series.only=FALSE) {
  
  h <- list(res=NA,type=NA,start=NA,end=NA,nas=NA,sep=NA)
  if(check.na | series.only | nlines<1000) {
    co <- file(file,encoding='ISO-8859-1')
    l <- readLines(con=co)
    close(co)
  } else {
    l <- c(scan(file,sep='\n',n=300,what='raw',blank.lines.skip=F,fileEncoding='ISO-8859-1',quiet=T),
           scan(file,sep='\n',n=-1L,what='raw',blank.lines.skip=F,fileEncoding='ISO-8859-1',skip=nlines-300,quiet=T))
  }
  
  # remove header and set field separator
  if(skip>=1L) {l <- l[-(1:skip)]}
  if(grepl(';',l[1],fixed=T)) {h$sep <- ';'}
  
  # remove first column (gauge id)
  l <- sub('^[[:digit:].]*?;','',l)
  
  # remove time interval if present
  if(grepl('-',l[1])) {
    h$type <- 'mean'
    l <- sub('-[[:digit:][:blank:].:]+;',';',l)
  } else {
    h$type <- 'peak'
  }
  
  # constrain date field to YYYY.MM.DD HH:MM
  if(grepl('^[[:digit:].]{7};',l[1])) {l <- sub(';','.01;',l,fixed=T)}
  l <- sub(';',' 00:00;',l,fixed=T)
  l <- sub('^([[:digit:].]+ [[:digit:]]{2}:[[:digit:]]{2}).*?([[:digit:].]+)$','\\1;\\2',l)
  
  # set missing values to NA
  i1 <- grepl('[[:digit:]]$',l)
  l[!i1] <- sub(';.*$',';NA',l[!i1])
  
  # remove leading and trailing NAs
  i2 <-  (cumsum(i1)>0) & rev(cumsum(rev(i1))>0)
  l <- l[i2]

  # check or return series
  if(!series.only) {
    sb <- ifelse(check.na|(nlines<1000),length(l),300-sum(!i2[1:(300-skip)])-skip)
    s <- bafuSeries(li=l,check.na=check.na,sblock=sb)
    h[names(s)] <- s
    return(h)
  } else {
    return(as.matrix(read.table(text=l,header=F,quote='',sep=';',as.is=T,row.names=1,col.names=c('x','y'),na.strings='NA')))
  }

}

bafuSeriesHydropro <-
function(file=NULL,nlines=0,skip=3,check.na=FALSE,series.only=FALSE) {
  
  h <- list(res=NA,type='unknown',start=NA,end=NA,nas=NA,sep=NA)
  if(check.na | series.only | nlines<1000) {
    co <- file(file,encoding='ISO-8859-1')
    l <- readLines(con=co)
    close(co)
  } else {
    l <- c(scan(file,sep='\n',n=300,what='raw',blank.lines.skip=F,fileEncoding='ISO-8859-1',quiet=T),
           scan(file,sep='\n',n=-1L,what='raw',blank.lines.skip=F,fileEncoding='ISO-8859-1',skip=nlines-300,quiet=T))
  }
  
  # remove header and set field separator
  if(skip>=1L) {l <- l[-(1:skip)]}
  if(grepl('\t',l[1],fixed=T)) {h$sep <- '\t'}
  l <- gsub(h$sep,';',l,fixed=T)
  
  # constrain date field to YYYY.MM.DD HH:MM
  if(grepl(';.*?;',l[1])) {l <- sub(';',' ',l,fixed=T)}
  l <- sub(' ([[:digit:]]{1}:[[:digit:]]{2});',' 0\\1;',l)
  l <- sub(';',' 00:00;',l,fixed=T)
  l <- sub('^([[:digit:].]+ [[:digit:]]{2}:[[:digit:]]{2}).*?([[:digit:].]+)$','\\1;\\2',l)
  if(grepl('^[[:digit:]]{2}\\.[[:digit:]]{2}\\.[[:digit:]]{4}',l[1])) {
    l <- sub('^([[:digit:]]{2})\\.([[:digit:]]{2})\\.([[:digit:]]{4})(.*)$','\\3.\\2.\\1 \\4',l)
  }
  
  # set missing values to NA
  i1 <- grepl('[[:digit:]]$',l)
  l[!i1] <- sub(';.*$',';NA',l[!i1])
  
  # remove leading and trailing NAs
  i2 <-  (cumsum(i1)>0) & rev(cumsum(rev(i1))>0)
  l <- l[i2]
  
  # check or return series
  if(!series.only) {
    sb <- ifelse(check.na|(nlines<1000),length(l),300-sum(!i2[1:(300-skip)])-skip)
    s <- bafuSeries(li=l,check.na=check.na,sblock=sb)
    h[names(s)] <- s
    return(h)
  } else {
    return(as.matrix(read.table(text=l,header=F,quote='',sep=';',as.is=T,row.names=1,col.names=c('x','y'),na.strings='NA')))
  }
  
}

bafuSeriesXls <-
function(file=NULL,nlines=0,skip=2,check.na=FALSE,series.only=FALSE) {
  
  h <- list(res=NA,type='peak',start=NA,end=NA,nas=NA,sep=NA)
  l <- unlist(gdata::read.xls(file,fileEncoding='ISO-8859-1',quote='"',header=F,sep='x',stringsAsFactors=F),use.names=F)
  
  # remove header and set field separator
  if(skip>=1L) {l <- l[-(1:skip)]}
  if(grepl(',',l[1],fixed=T)) {h$sep <- ','}
  l <- gsub(h$sep,';',l,fixed=T)
  
  # remove first unnecessary columns
  l <- sub('^.*?;([[:digit:][:blank:].:]*?;.*$)','\\1',l)
  
  # switch remaining two columns
  l <- sub('^(.*?);(.*?)$','\\2;\\1',l)
  
  # constrain date field to YYYY.MM.DD HH:MM
  l <- gsub('/','.',l,fixed=T)
  l <- sub('NA.NA','00.00',l,fixed=T)
  l <- sub('^([[:digit:]]{1})\\.','0\\1.',l)
  l <- sub('\\.([[:digit:]]{1})\\.','.0\\1.',l)
  l <- sub('^([[:digit:]]{2})\\.([[:digit:]]{2})\\.([[:digit:]]{4});(.*)$','\\3.\\1.\\2;\\4',l)
  l <- sub(';',' 00:00;',l,fixed=T)
  l <- sub('^([[:digit:].]+ [[:digit:]]{2}:[[:digit:]]{2}).*?([[:digit:].]+)$','\\1;\\2',l)
  
  # set missing values to NA
  i1 <- grepl('[[:digit:]]$',l)
  l[!i1] <- sub(';.*$',';NA',l[!i1])
  
  # remove leading and trailing NAs
  i2 <-  (cumsum(i1)>0) & rev(cumsum(rev(i1))>0)
  l <- l[i2]
  
  # check or return series
  if(!series.only) {
    sb <- ifelse(check.na|(nlines<1000),length(l),300-sum(!i2[1:(300-skip)])-skip)
    s <- bafuSeries(li=l,check.na=check.na,sblock=sb)
    h[names(s)] <- s
    return(h)
  } else {
    return(as.matrix(read.table(text=l,header=F,quote='',sep=';',as.is=T,row.names=1,col.names=c('x','y'),na.strings='NA')))
  }
  
}