.sniffBafuFile <- function(f) {
  
  h <- list(type=NA,id=NA,start=NA,end=NA,skip=0,sep=NA,var=NA,time='irregular',comment=NA)
  h['type'] <- sub('^.*?(\\..*$)','\\1',f)
  
  if(!grepl('\\.xls$',f)) {
    
    n <- capture.output(scan(f,sep='\n',what=list(NULL),blank.lines.skip=F),type='message')
    n <- as.numeric(sub('^.*?([[:digit:]]+).*$','\\1',n))
    l1 <- scan(f,sep='\n',n=30,what='raw',blank.lines.skip=F,fileEncoding='ISO-8859-1',quiet=T)
    l1 <- gsub('(\\*|%)','',l1)
    l2 <- scan(f,sep='\n',n=1,what='raw',blank.lines.skip=F,fileEncoding='ISO-8859-1',skip=n-1,quiet=T)
    n <- grep('^[[:blank:][:digit:];.:-]+$',l1)[1]
    h['skip'] <- n-1
    l1[1:(n-1)] <- sub(' / donn.*?onal data ','',l1[1:(n-1)])
    l1[1:(n-1)] <- sub('^[ ]+','',l1[1:(n-1)])
    l1[1:(n-1)] <- sub('[ ]+$','',l1[1:(n-1)])
    h['comment'] <- paste(l1[1:(n-1)],collapse=' ')
    
    if(grepl(';',l1[n])) {
      
      h['sep'] <- ';'
      h['id'] <- sub('^([[:digit:]]+);.*$','\\1',l1[n])
      d <- sub('^.*?;(.*?);.*$','\\1',c(l1[n:length(l1)],l2))
      
      if(grepl('-',d[1])) {
        
        h['var'] <-'mean'
        d <- sub('^(.*?)-.*$','\\1',d)
        if (nchar(d[1])<10) {d <- paste(d,'01',sep='.')}
        tf <- ifelse(grepl('^[[:digit:].]{10} [[:digit:]:]+$',d[1]),'%Y.%m.%d %H:%M','%Y.%m.%d')
        d <- as.POSIXct(d,format=tf,tz='UTC')
        dd <- diff(d[-length(d)])
        if (grepl('min',attr(dd,'units')) & all(dd==10)) {h['time'] <- '10min'; h[c('start','end')] <- format(d[c(1,length(d))],'%Y-%m-%d %H:%M',tz='UTC')}
        if (grepl('hour',attr(dd,'units')) & all(dd==1)) {h['time'] <- 'hour'; h[c('start','end')] <- format(d[c(1,length(d))],'%Y-%m-%d %H',tz='UTC')}
        if (grepl('day',attr(dd,'units')) & all(dd==1)) {h['time'] <- 'day'; h[c('start','end')] <- format(d[c(1,length(d))],'%Y-%m-%d',tz='UTC')}
        if (grepl('day',attr(dd,'units')) & all(dd<32 & dd>27)) {h['time'] <- 'month'; h[c('start','end')] <- format(d[c(1,length(d))],'%Y-%m',tz='UTC')}
        
      } else {
        
        h['var'] <- 'peak'
        d <- as.POSIXct(d,format='%Y.%m.%d %H:%M',tz='UTC')
        h[c('start','end')] <- format(d[c(1,length(d))],'%Y.%m.%d %H:%M:%S',tz='UTC')
        dd <- sub('[[:digit:]]{2}:[[:digit:]]{2}$','00',d)
        dd <- as.POSIXct(dd,format='%Y-%m-%d %H:%M',tz='UTC')
        dd <- diff(dd[-length(dd)])
        if (grepl('hour',attr(dd,'units')) & all(dd==1)) {h['time'] <- 'hour'; h[c('start','end')] <- format(d[c(1,length(d))],'%Y-%m-%d %H',tz='UTC')}
        dd <- sub(' [[:digit:]:]+$','',d)
        dd <- as.POSIXct(dd,format='%Y-%m-%d',tz='UTC')
        dd <- diff(dd[-length(dd)])
        if (grepl('day',attr(dd,'units')) & all(dd==1)) {h['time'] <- 'day'; h[c('start','end')] <- format(d[c(1,length(d))],'%Y-%m-%d',tz='UTC')}
        dd <- sub('\\.[[:digit:]]+ [[:digit:]:]+$','.01',d)
        dd <- as.POSIXct(dd,format='%Y-%m-%d',tz='UTC')
        dd <- diff(dd[-length(dd)])
        if (grepl('day',attr(dd,'units')) & all(dd<65 & dd>1)) {h['time'] <- 'month'; h[c('start','end')] <- format(d[c(1,length(d))],'%Y-%m',tz='UTC')}
        
      }
      
    } else {
      
      h['sep'] <- '\t'
      h['var'] <-'unknown'
      i <- grep('^HBCHa',l1)
      h['id'] <- sub('HBCHa-([[:digit:]]+)-.*','\\1',l1[i])
      d <- sub('\t[[:digit:].,]+$','',c(l1[n:length(l1)],l2))
      d <- sub('\t',' ',d)
      tf <- ifelse(grepl('^[[:digit:].]{10} [[:digit:]:]+$',d[1]),'%d.%m.%Y %H:%M','%d.%m.%Y')
      d <- as.POSIXct(d,format=tf,tz='UTC')
      dd <- diff(d[-length(d)])
      if (grepl('min',attr(dd,'units')) & all(dd==10)) {h['time'] <- '10min'; h[c('start','end')] <- format(d[c(1,length(d))],'%Y-%m-%d %H:%M',tz='UTC')}
      if (grepl('hour',attr(dd,'units')) & all(dd==1)) {h['time'] <- 'hour'; h[c('start','end')] <- format(d[c(1,length(d))],'%Y-%m-%d %H',tz='UTC')}
      if (grepl('day',attr(dd,'units')) & all(dd==1)) {h['time'] <- 'day'; h[c('start','end')] <- format(d[c(1,length(d))],'%Y-%m-%d',tz='UTC')}
      if (grepl('day',attr(dd,'units')) & all(dd<32 & dd>27)) {h['time'] <- 'month'; h[c('start','end')] <- format(d[c(1,length(d))],'%Y-%m',tz='UTC')}
      
    }
    
  } else {
    
    l <- gdata::read.xls(f,fileEncoding='ISO-8859-1',quote='"',nrows=1,header=F,sep=';',stringsAsFactors=F)[1,1]
    l <- gsub('[\\"]','',l)
    h['skip'] <- 1
    h['sep'] <- ','
    h['comment'] <- sub(',,,$','',l)
    l <- gdata::read.xls(f,fileEncoding='ISO-8859-1',quote='"',header=T,sep=',',stringsAsFactors=F,skip=1)
    h['id'] <- l[1,'KennNr']
    h['var'] <- ifelse(grepl('-',l[1,'Datum']),'mean','peak')
    h['time'] <- 'year'
    h['start'] <- l[1,'Jahr']
    h['end'] <- l[nrow(l),'Jahr']
    
  }
  
  return(h)
  
}