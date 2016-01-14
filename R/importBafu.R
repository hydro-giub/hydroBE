importBafu <- 
function(dir=NULL,files=NULL) {
  
  
  
  source('/home/sschick/workspace/hydroBE/R/sniffBafuFile.R')
  dir <- '/home/sschick/workspace/test/hq-mm/'
  
  if(!is.null(dir)) {files <- list.files(dir,full.names=T)}
  df <- data.frame(file=files,type=NA,id=NA,start=NA,end=NA,skip=0,sep=NA,var=NA,time=NA,comment=NA,stringsAsFactors=F)
  for (i in 1:nrow(df)) {df[i,2:10] <- sniffBafuFile(f=files[i])}
  if(length(setdiff(unique(df$var),'unknown'))>1 | length(unique(df$time))!=1) {
    stop('variables (peak vs. mean values) or time units in the provided files are not consistent')
  }
  
  tf <- rep(NA,nrow(df))
  tf[grepl('^[[:digit:]]{4}$',df$start)] <- '%Y'
  tf[grepl('^[[:digit:]]{4}-[[:digit:]]{2}$',df$start)] <- '%Y-%m'
  tf[grepl('^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}$',df$start)] <- '%Y-%m-%d'
  tf[grepl('^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2} [[:digit:]]{2}$',df$start)] <- '%Y-%m-%d %H'
  tf[grepl('^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2} [[:digit:]]{2}:[[:digit:]]{2}$',df$start)] <- '%Y-%m-%d %H:%M'
  tf[grepl('^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2} [[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}$',df$start)] <- '%Y-%m-%d %H:%M:%S'
  if(length(unique(tf))!=1) {stop('time accuracy is inconsistent')} else {tf <- unique(tf)}
  
  if(tf=='%Y') {
    r <- range(df$start,df$end)
    df1 <- data.frame(time=seq.int(from=r[1],to=r[2],by=1))
  }
  if(tf=='%Y-%m') {
    r <- range(as.numeric(sub('-.*$','',df$start)),as.numeric(sub('-.*$','',df$end)))
    i <- rep(seq(from=r[1],to=r[2],by=1),each=12)
    i <- paste(i,formatC(rep(1:12,times=length(i)/12),flag='0',width=2),sep='-')
    r <- range(match(c(df$start,df$end),i))
    df1 <- data.frame(time=i[r[1]:r[2]])
  }
  if(grepl('%Y-%m-%d',tf)) {
    df$start <- as.POSIXct(df$start,format=tf,tz='UTC')
    df$end <- as.POSIXct(df$end,format=tf,tz='UTC')
    r <- range(df$start,df$end)
    df1 <- data.frame(time=seq.POSIXt(from=r[1],to=r[2],by=df$time[1],tz='UTC'))
  }
  
  for (i in 1:nrow(df)) {
    
    isHp <- grepl('[Hh]ydro[Pp]ro',df$comment[i])
    isXls <- grepl('xls',df$type[i])
    
    if(isHp & !isXls) {
      df2 <- read.table(df$file[i],header=F,sep=df$sep[i],quote='',skip=df$skip[i],stringsAsFactors=F)
    }
    
    if(!isHp & isXls) {
      
    }
    
    if(!isHp & !isXls) {
      
    }
    
    # merge with df1
    
  }
  
  
  # merge orders splitted on different files
  
  return(df)
  
}