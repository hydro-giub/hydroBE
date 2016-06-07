checkSeries <-
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

getTimestamps <-
function(res=NULL,sm=NULL) {
  
  if(res=='year') {
    r <- range(as.numeric(sub('\\..*$','',sm[,'start'])),as.numeric(sub('\\..*$','',sm[,'end'])))
    x <- seq.int(from=r[1],to=r[2],by=1)
    n <- 4
  }
  if(res=='month') {
    r <- range(as.numeric(sub('\\..*$','',sm[,'start'])),as.numeric(sub('\\..*$','',sm[,'end'])))
    i <- rep(seq(from=r[1],to=r[2],by=1),each=12)
    i <- paste(i,formatC(rep(1:12,times=length(i)/12),flag='0',width=2),sep='.')
    r <- range(match(c(sm[,'start'],sm[,'end']),i))
    x <- i[r[1]:r[2]]
    n <- 7
  }
  if(res=='day') {
    r <- range(as.POSIXct(as.vector(sm[,c('start','end')]),format='%Y.%m.%d',tz='UTC'))
    x <- format(seq.POSIXt(from=r[1],to=r[2],by='day',tz='UTC'),'%Y.%m.%d')
    n <- 10
  }
  if(res=='hour') {
    r <- range(as.POSIXct(as.vector(sm[,c('start','end')]),format='%Y.%m.%d %H',tz='UTC'))
    x <- format(seq.POSIXt(from=r[1],to=r[2],by='hour',tz='UTC'),'%Y.%m.%d %H')
    n <- 13
  }
  if(res=='10 min') {
    r <- range(as.POSIXct(as.vector(sm[,c('start','end')]),format='%Y.%m.%d %H:%M',tz='UTC'))
    x <- format(seq.POSIXt(from=r[1],to=r[2],by='10 min',tz='UTC'),'%Y.%m.%d %H:%M')
    n <- 16
  }
  
  return(list(x=x,n=n))
  
}