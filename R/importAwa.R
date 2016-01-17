importAwa <-
function(dir=NULL,files=NULL,agg='day',quiet=TRUE) {
  
  if(!is.null(dir)) {files <- list.files(dir,full.names=T)}
  df <- data.frame(file=files,type=NA,id=NA,start=NA,end=NA,skip=0,sep=NA,var=NA,time=NA,unit=NA,comment=NA,stringsAsFactors=F)
  for (i in 1:nrow(df)) {df[i,2:11] <- sniffAwaFile(f=files[i])}
  r <- range(as.POSIXct(df$start,format='%d.%m.%Y %H:%M',tz='UTC'),as.POSIXct(df$end,format='%d.%m.%Y %H:%M',tz='UTC'))
  
  if(grepl('[[:digit:]]+[ ]*[mM]in',agg)) {
    r <- as.POSIXct(sub('[[:digit:]]{1}:[[:digit:]]{2}$','0:00',as.character(r)),tz='UTC')
    df1 <- data.frame(time=seq.POSIXt(from=r[1],to=r[2],by='10 min'))
  }
  if(grepl('[hH]our',agg)) {
    r <- as.POSIXct(sub('[[:digit:]]{2}:[[:digit:]]{2}$','00:00',as.character(r)),tz='UTC')
    df1 <- data.frame(time=seq.POSIXt(from=r[1],to=r[2],by='hour'))
  }
  if(grepl('[dD]ay',agg)) {
    r <- as.POSIXct(format(r,'%Y-%m-%d'),tz='UTC')
    df1 <- data.frame(time=seq.POSIXt(from=r[1],to=r[2],by='day'))
  }
  if(grepl('[mM]onth',agg)) {
    r <- format(r,'%Y-%m')
    i <- range(as.numeric(sub('-.*$','',r)))
    i <- rep(seq(from=i[1],to=i[2],by=1),each=12)
    i <- paste(i,formatC(rep(1:12,times=length(i)/12),flag='0',width=2),sep='-')
    r <- range(match(r,i))
    df1 <- data.frame(time=i[r[1]:r[2]])
  }
  if(grepl('[yY]ear',agg)) {
    r <- as.integer(format(r,'%Y'))
    df1 <- data.frame(time=seq.int(from=r[1],to=r[2],by=1))
  }

  for (i in 1:nrow(df)) {
    df2 <- read.table(file=df$file[i],header=F,sep=df$sep[i],skip=df$skip[i],stringsAsFactors=F,strip.white=T,
                      na.strings=c('NA','Luecke','L\u00FCcke','L\xfccke',''),encoding='CP1282')
    df2[,1] <- as.POSIXct(paste(df2[,1],df2[,2]),format='%d.%m.%Y %H:%M',tz='UTC')
    df2 <- df2[,c(1,3)]
    if(grepl('[[:digit:]]+[ ]*[mM]in',agg)) {
      ag <- tapply(df2[,2],sub('[[:digit:]]{1}:[[:digit:]]{2}$','0:00',as.character(df2[,1])),mean)
      df2 <- data.frame(x=as.POSIXct(names(ag),tz='UTC'),y=ag)
    }
    if(grepl('[hH]our',agg)) {
      ag <- tapply(df2[,2],sub('[[:digit:]]{2}:[[:digit:]]{2}$','00:00',as.character(df2[,1])),mean)
      df2 <- data.frame(x=as.POSIXct(names(ag),tz='UTC'),y=ag)
    }
    if(grepl('[dD]ay',agg)) {
      ag <- tapply(df2[,2],format(df2[,1],'%Y-%m-%d',tz='UTC'),mean)
      df2 <- data.frame(x=as.POSIXct(names(ag),tz='UTC'),y=ag)
    }
    if(grepl('[mM]onth',agg)) {
      ag <- tapply(df2[,2],format(df2[,1],'%Y-%m',tz='UTC'),mean)
      df2 <- data.frame(x=names(ag),y=ag)
    }
    if(grepl('[yY]ear',agg)) {
      ag <- tapply(df2[,2],format(df2[,1],'%Y',tz='UTC'),mean)
      df2 <- data.frame(x=as.integer(names(ag)),y=ag)
    }
    names(df2) <- c('time',paste('a',i,'a',df$id[i],sep=''))
    df1 <- merge(x=df1,y=df2,by='time',all.x=T,sort=T)
  }
  
  cn <- sub('^a[[:digit:]]+a','',names(df1))
  ci <- 1:nrow(df)
  i <- 3
  while (i<=ncol(df1)) {
    ii <- which(cn[i]==cn[1:(i-1)])
    ii <- ii[df$unit[ci[ii-1]]==df$unit[ci[i-1]]]
    if(length(ii)==1) {
      rpl <- is.na(df1[,ii]) & !is.na(df1[,i])
      df1[rpl,ii] <- df1[rpl,i]
      df1 <- df1[,-i]
      cn <- cn[-i]
      ci <- ci[-(i-1)]
    } else {i <- i+1}
  }
  names(df1) <- c('time',paste(df$unit[ci],cn[-1],sep=''))
  
  if(!quiet) {print(df)}
  return(df1)
  
}