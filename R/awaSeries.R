awaSeriesHydropro <-
function(file=NULL,nlines=0,skip=3,check.na=FALSE,series.only=FALSE) {
  
  h <- list(res=NA,type='unknown',start=NA,end=NA,nas=NA,sep=NA)
  if(check.na | series.only | nlines<1000) {
    co <- file(file,encoding='CP1282')
    l <- readLines(con=co)
    close(co)
  } else {
    l <- c(scan(file,sep='\n',n=300,what='raw',blank.lines.skip=F,fileEncoding='CP1282',quiet=T),
           scan(file,sep='\n',n=-1L,what='raw',blank.lines.skip=F,fileEncoding='CP1282',skip=nlines-300,quiet=T))
  }
  
  # remove header and set field separator
  if(skip>=1L) {l <- l[-(1:skip)]}
  if(grepl('\t',l[1],fixed=T)) {h$sep <- '\t'}
  if(grepl(',',l[1],fixed=T)) {h$sep <- ','}
  l <- gsub(h$sep,';',l,fixed=T)
  
  # retain only first three columns
  l <- sub('^([[:digit:]:.]+;[[:digit:]:.]+;[[:digit:]:.]+).*$','\\1',l)
  
  # constrain date field to YYYY.MM.DD HH:MM
  if(grepl(';.*?;',l[1])) {l <- sub(';',' ',l,fixed=T)}
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