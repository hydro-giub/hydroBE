importAwa <-
function(dir=NULL,files=NULL,quiet=TRUE) {
  
  if(!is.null(dir)) {files <- list.files(dir,full.names=T)}
  df <- data.frame(file=files,type=NA,id=NA,start=NA,end=NA,skip=0,sep=NA,var=NA,time=NA,unit=NA,comment=NA,stringsAsFactors=F)
  for (i in 1:nrow(df)) {df[i,2:11] <- sniffAwaFile(f=files[i])}
  if(length(unique(df$time))!=1) {stop('time units in the provided files are not consistent')}
  
  
}