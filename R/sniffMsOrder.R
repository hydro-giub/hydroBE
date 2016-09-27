sniffMsOrder <- function(dir=NULL,v=NULL,per=NULL) {

  # check legend files
  f <- list.files(dir,pattern='\\.zip$',full.names=T)
  h <- c('id','site','variable','source','xWGS84','yWGS84','xLV1903','yLV1903','altitude','file')
  m <- matrix(NA,nrow=0,ncol=length(h),dimnames=list(NULL,h))
  for (i in f) {
    ff <- unzip(i,list=T)
    ff <- ff$Name[grepl('^order_[0-9]+_legend\\.txt$', ff$Name)]
    c <- unz(i,ff)
    l <- readLines(c)
    close(c)
    l <- iconv(l,from='latin1',to='UTF-8')
    s <- grep('stn +Name',l)+1
    e <- grep('^Parameter',l)-2
    l <- l[s:e]
    l <- strsplit(l,'  ')
    l <- unlist(lapply(l,function(x){x<-x[nchar(x)>0];sub('^ ','',x)}))
    l <- matrix(l,ncol=7,byrow=T)
    l <- cbind(l[,1:4],matrix(unlist(strsplit(l[,5],'/')),ncol=2,byrow=T),matrix(unlist(strsplit(l[,6],'/')),ncol=2,byrow=T),l[,7],rep(basename(i),nrow(l)))
    m <- rbind(m,l)
  }
  m <- m[!grepl('unknown',m[,'source'],fixed=T),]
  
  # remove duplicates
  isD <- rep(F,nrow(m))
  for (i in 1:nrow(m)) {
    if (isD[i]) {next}
    id <- m[i,'id']
    ii <- grep(paste('^',id,'$',sep=''),m[,'id'])
    if (length(ii>1)) {
      for (j in setdiff(ii,i)) {
        if(all(m[j,c('id','site','xLV1903','yLV1903','altitude','file')]==m[i,c('id','site','xLV1903','yLV1903','altitude','file')])) {
          isD[j] <- T
          m[i,'variable'] <- paste(m[i,'variable'],m[j,'variable'],sep=',')
        }
      }
    }
  }
  m <- m[!isD,]
  
  # check period
  if (!is.null(per)) {
    l <- ff <- f1 <- f2 <- ''
    m <- cbind(m,matrix(F,ncol=length(v),nrow=nrow(m),dimnames=list(NULL,paste(v,'Ok',sep=''))))
    for (i in 1:nrow(m)) {
      f1 <- paste(dir,m[i,'file'],sep='')
      if (f1!=f2) {
        f2 <- f1
        ff <- unzip(f1,list=T)
        ff <- ff$Name[grepl('^order_[0-9]+_data\\.txt$', ff$Name)]
        c <- unz(f2,ff)
        l <- readLines(c,encoding='latin1')
        close(c)
      }
      ii <- range(grep(paste('^',m[i,'id'],sep=''),l))
      df <- read.table(unz(f2,ff),header=T,sep=';',quote='',na.strings='-',skip=ii[1]-2,nrows=diff(ii)+1,blank.lines.skip=F,stringsAsFactors=F)
      df$time <- as.Date(as.character(df$time),'%Y%m%d')
      for (j in v) {
        jj <- grep(j,names(df))
        if (length(jj)!=1) {next}
        isNum <- !is.na(df[,jj])
        isNum <- (cumsum(isNum)>0) & rev(cumsum(rev(isNum))>0)
        r <- range(df$time[isNum])
        if (r[1]<=per[1] & r[2]>=per[2]) {
          if (all(!is.na(df[df$time>=per[1] & df$time<=per[2],jj]))) {m[i,paste(j,'Ok',sep='')] <- T}
        } 
      }
    }
  }
  
  return(m)

}
