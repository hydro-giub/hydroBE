importAwa <-
function(dir=NULL,files=NULL,agg='day',quiet=TRUE) {
  
  dir='/home/sschick/workspace/test/'
  source('/home/sschick/workspace/hydroBE/R/sniffAwaFile.R')
  
  
  if(!is.null(dir)) {files <- list.files(dir,full.names=T)}
  df <- data.frame(file=files,type=NA,id=NA,start=NA,end=NA,skip=0,sep=NA,var=NA,time=NA,unit=NA,comment=NA,stringsAsFactors=F)
  for (i in 1:nrow(df)) {df[i,2:11] <- sniffAwaFile(f=files[i])}
  if(length(unique(df$time))!=1) {stop('time units in the provided files are not consistent')}
  
  
  r <- range(as.POSIXct(df$start,format='%d.%m.%Y %H:%M',tz='UTC'),as.POSIXct(df$end,format='%d.%m.%Y %H:%M'),tz='UTC')
  df1 <- data.frame(time=NA)
  
}