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
    s <- checkSeries(li=l,check.na=check.na,sblock=sb)
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
    s <- checkSeries(li=l,check.na=check.na,sblock=sb)
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
    s <- checkSeries(li=l,check.na=check.na,sblock=sb)
    h[names(s)] <- s
    return(h)
  } else {
    return(as.matrix(read.table(text=l,header=F,quote='',sep=';',as.is=T,row.names=1,col.names=c('x','y'),na.strings='NA')))
  }
  
}