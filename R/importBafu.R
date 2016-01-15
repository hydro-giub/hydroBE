importBafu <- 
function(dir=NULL,files=NULL,quiet=TRUE) {

  if(!is.null(dir)) {files <- list.files(dir,full.names=T)}
  df <- data.frame(file=files,type=NA,id=NA,start=NA,end=NA,skip=0,sep=NA,var=NA,time=NA,unit=NA,comment=NA,stringsAsFactors=F)
  for (i in 1:nrow(df)) {df[i,2:11] <- sniffBafuFile(f=files[i])}
  if(length(unique(df$time))!=1) {stop('time units in the provided files are not consistent')}
  
  tf <- rep(NA,nrow(df))
  tf[grepl('^[[:digit:]]{4}$',df$start)] <- '%Y'
  tf[grepl('^[[:digit:]]{4}-[[:digit:]]{2}$',df$start)] <- '%Y-%m'
  tf[grepl('^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}$',df$start)] <- '%Y-%m-%d'
  tf[grepl('^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2} [[:digit:]]{2}$',df$start)] <- '%Y-%m-%d %H'
  tf[grepl('^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2} [[:digit:]]{2}:[[:digit:]]{2}$',df$start)] <- '%Y-%m-%d %H:%M'
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
      df2 <- read.table(df$file[i],sep=df$sep[i],skip=df$skip[i],quote='',header=F,stringsAsFactors=F,strip.white=T,
                        na.strings=c('NA','Luecke','L\u00FCcke','L\xfccke',''),encoding='ISO-8859-1')
      df2[,1] <- paste(df2[,1],df2[,2])
      df2 <- df2[,-2]
      tff <- sub('%Y-','%d-',tf)
      tff <- sub('-%d','-%Y',tff)
    }
    
    if(!isHp & isXls) {
      df2 <- gdata::read.xls(df$file[i],sep=df$sep[i],skip=df$skip[i],quote='"',header=T,stringsAsFactors=F,strip.white=T,
                             na.strings=c('NA','Luecke','L\u00FCcke','L\xfccke',''),fileEncoding='ISO-8859-1')
      df2 <- df2[,2:3]
      tff <- tf
    }
    
    if(!isHp & !isXls) {
      df2 <- read.table(df$file[i],sep=df$sep[i],skip=df$skip[i],quote='',header=F,stringsAsFactors=F,strip.white=T,
                        na.strings=c('NA','Luecke','L\u00FCcke','L\xfccke',''),encoding='ISO-8859-1')
      df2 <- df2[,-1]
      tff <- tf
    }
    
    df2[,1] <- sub('-.*$','',df2[,1])
    df2[,1] <- gsub('\\.','-',df2[,1])
    if(tf=='%Y') {df2[,1] <- sub('^([[:digit:]]{4}).*$','\\1',df2[,1])}
    if(tf=='%Y-%m') {df2[,1] <- sub('^([[:digit:]]{4}-[[:digit:]]{2}).*$','\\1',df2[,1])}
    if(grepl('%Y-%m-%d',tf)) {df2[,1] <- as.POSIXct(df2[,1],format=tff,tz='UTC')}
    names(df2) <- c('time',paste('a',i,'a',df$id[i],sep=''))
    df1 <- merge(x=df1,y=df2,by='time',all.x=T,sort=T)
    
  }
  
  cn <- sub('^a[[:digit:]]+a','',names(df1))
  ci <- 1:nrow(df)
  i <- 3
  while (i<=ncol(df1)) {
    ii <- which(cn[i]==cn[1:(i-1)])
    ii <- ii[df$var[ii-1]==df$var[i-1] & df$unit[ii-1]==df$unit[i-1]]
    if(length(ii)==1) {
      rpl <- is.na(df1[,ii]) & !is.na(df1[,i])
      df1[rpl,ii] <- df1[rpl,i]
      df1 <- df1[,-i]
      cn <- cn[-i]
      ci <- ci[-(i-1)]
    } else {i <- i+1}
  }
  names(df1) <- c('time',paste(df$unit[ci],df$var[ci],cn[-1],sep=''))
  
  if(!quiet) {print(df)}
  return(df1)
  
}