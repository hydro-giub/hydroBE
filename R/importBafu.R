importBafu <- 
function(dir=NULL,files=NULL) {
  
  if(!is.null(dir)) {files <- list.files(dir,full.names=T)}
  df <- data.frame(file=files,type=NA,id=NA,start=NA,end=NA,skip=0,sep=NA,var=NA,time=NA,comment=NA,stringsAsFactors=F)
  for (i in 1:nrow(df)) {df[i,2:10] <- .sniffBafuFile(f=files[i])}
  
  # check consistency (time units)
  # prepare df
  # read in and merge (handle splitted orders according order in dir/files!)
  
  return(df)
  
}