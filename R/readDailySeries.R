readDailySeries <- function(file=NULL,id=NULL,provider=NULL,series=TRUE,vars=NULL) {

    provider <- tolower(provider)

    p <- dirname(file)
    f <- basename(file)
    file <- list.files(path=p,pattern=f,full.names=TRUE)
    if(length(file)!=1L) {
        warning('file not found or multiple matches')
        return(NULL)
    }
    
    
    if(provider=='bafu') {
        m <- .readDailySeriesBafu(file=file,id=id,series=series)
    }

    if(provider=='bmlfuw') {
        m <- .readDailySeriesBmlfuw(file=file,id=id,series=series)
    }

    if(provider=='grdc') {
        m <- .readDailySeriesGrdc(file=file,id=id,series=series)
    }

    if(provider=='lfub') {
        m <- .readDailySeriesLfub(file=file,id=id,series=series)
    }

    if(provider=='lubw') {
        m <- .readDailySeriesLubw(file=file,id=id,series=series)
    }

    if(provider=='mapama') {
        m <- .readDailySeriesMapama(file=file,id=id,series=series)
    }

    if(provider=='medde') {
        m <- .readDailySeriesMedde(file=file,id=id,series=series)
    }

    if(provider=='metsw') {
        m <- .readDailySeriesMetsw(file=file,id=id,series=series,vars=vars)
    }

    return(m)

}


.readDailySeriesBafu <- function(file=NULL,id=NULL,series=TRUE) {
    
    l <- readLines(file,encoding='latin1',n=ifelse(series,-1L,200L))
    n <- length(l)
    ll <- l[1:ifelse(n>200,200L,n)]
    
    h <- ll[grepl(paste0('^[[:digit:]]+ / ',id,'.*$'),ll)]
    site <- sub('.*?-(.*) [[:digit:]]+.*','\\1',h)
    name <- sub('.*[[:digit:]]+ (.*?)-.* [[:digit:]]+.*','\\1',h)
    id2 <- sub('.*/ ([[:digit:]]+) .*','\\1',h)
    skip <- grep(paste0(id,';[-[:digit:];,. ]+$'),ll)[1L]-1L

    if(series) {
        
        l <- l[-(1:skip)]
        l <- sub('^[[:digit:]]+;','',l)
        d <- sub('^([[:digit:]]{4}\\.[[:digit:]]{2}\\.[[:digit:]]{2}).*$','\\1',l)
        d <- gsub('.','-',d,fixed=TRUE)
        l <- sub('^.*;(.*)$','\\1',l)
        m <- matrix(as.numeric(l),nrow=length(l),ncol=1L,dimnames=list(d,'qd'))
        isNum <- !is.na(m[,1L])
        isNum <- (cumsum(isNum)>0) & rev(cumsum(rev(isNum))>0)
        m <- m[isNum,,drop=FALSE]
        if(any(diff(as.Date(rownames(m)))!=1L)) {
            warning('irregular series',immediate.=TRUE)
        }
        
    } else {
        
        m <- list(file=file,name=name,site=site,id=id,skip=skip)
        
    }
    
    if(id2!=id && as.integer(id2) != as.integer(id)) {
        warning('id does not match',immediate.=TRUE)
    }
    
    return(m)
    
}

.readDailySeriesBmlfuw <- function(file=NULL,id=NULL,series=TRUE) {

    l <- readLines(file,encoding='latin1',n=ifelse(series,-1L,200L))
    n <- length(l)
    ll <- l[1:ifelse(n>200,200L,n)]
    
    site <- ll[grepl('^Messstelle: *;.*$',ll)]
    site <- sub('^Messstelle: *;(.*)$','\\1',site)
    name <- ll[grepl('^Gew\u00E4sser: *;.*$',ll)]
    name <- sub('^Gew\u00E4sser: *;(.*)$','\\1',name)
    id2 <- ll[grepl('^HZB-Nummer: *;.*$',ll)]
    id2 <- sub('^HZB-Nummer: *;(.*)$','\\1',id2)
    skip <- grep('^Werte:$',ll)[1L]

    if(series) {
        
        l <- l[-(1:skip)]
        d <- sub('^([[:digit:]]{2})\\.([[:digit:]]{2})\\.([[:digit:]]{4}).*$','\\3-\\2-\\1',l)
        l <- sub('^.*;(.*)$','\\1',l)
        m <- matrix(as.numeric(l),nrow=length(l),ncol=1L,dimnames=list(d,'qd'))
        isNum <- !is.na(m[,1L])
        isNum <- (cumsum(isNum)>0) & rev(cumsum(rev(isNum))>0)
        m <- m[isNum,,drop=FALSE]
        if(any(diff(as.Date(rownames(m)))!=1L)) {
            warning('irregular series',immediate.=TRUE)
        }
        
    } else {
        
        m <- list(file=file,name=name,site=site,id=id,skip=skip)
        
    }

    if(id2!=id && as.integer(id2) != as.integer(id)) {
        warning('id does not match',immediate.=TRUE)
    }
    
    return(m)

}

.readDailySeriesGrdc <- function(file=NULL,id=NULL,series=TRUE) {

    l <- readLines(file,encoding='latin1',n=ifelse(series,-1L,200L))
    n <- length(l)
    ll <- l[1:ifelse(n>200,200L,n)]

    site <- ll[grep('Station:',ll,fixed=TRUE)]
    site <- sub('^.*?Station:[[:blank:]]*(.*)$','\\1',site)
    name <- ll[grep('River:',ll,fixed=TRUE)]
    name <- sub('^.*?River:[[:blank:]]*(.*)$','\\1',name)
    id2 <- ll[grep('GRDC-No.:',ll,fixed=TRUE)]
    id2 <- sub('^.*?GRDC-No\\.:[[:blank:]]*(.*)$','\\1',id2)
    skip <- grep('^[[:digit:]]+[[:digit:][:punct:][:blank:]]+$',ll)[1L]-1L

    if(series) {
        
        l <- l[-(1:skip)]
        d <- sub('^([[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}).*$','\\1',l)
        l <- sub(';[^;]*$','',l)
        l <- sub('^.*;(.*)$','\\1',l)
        m <- matrix(as.numeric(l),nrow=length(l),ncol=1L,dimnames=list(d,'qd'))
        m[m[,1L]<0,1L] <- NA
        isNum <- !is.na(m[,1L])
        isNum <- (cumsum(isNum)>0) & rev(cumsum(rev(isNum))>0)
        m <- m[isNum,,drop=FALSE]
        if(any(diff(as.Date(rownames(m)))!=1L)) {
            warning('irregular series',immediate.=TRUE)
        }
        
    } else {
        
        m <- list(file=file,name=name,site=site,id=id,skip=skip)
        
    }

    if(id2!=id && as.integer(id2) != as.integer(id)) {
        warning('id does not match',immediate.=TRUE)
    }
    
    return(m)

}

.readDailySeriesLfub <- function(file=NULL,id=NULL,series=TRUE) {
    
    l <- readLines(file,encoding='latin1',n=ifelse(series,-1L,200L))
    n <- length(l)
    ll <- l[1:ifelse(n>200,200L,n)]

    site <- ll[grepl('^Messstellen-Name[:;\"]*.*?[.;\"]*$',ll)]
    site <- sub('^Messstellen-Name[:;\"]*(.*?)[.;\"]*$','\\1',site)
    name <- ll[grepl('^Gew\u00E4sser[:;\"]*.*?[:;\"]*$',ll)]
    name <- sub('^Gew\u00E4sser[:;\"]*(.*?)[:;\"]*$','\\1',name)
    id2 <- ll[grepl('^Messstellen-Nr[.:;\"]*[0-9]+$',ll)]
    id2 <- sub('^Messstellen-Nr[.:;\"]*([0-9]+)$','\\1',id2)
    skip <- grep('^Datum;',ll)[1L]

    if(series) {
        
        l <- l[-(1:skip)]
        d <- sub('^([[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}).*$','\\1',l)
        l <- sub('^[-[:digit:]]+;([[:digit:],]+);.*$','\\1',l)
        l <- sub('^.*;(.*)$','\\1',l)
        l <- sub(',','.',l,fixed=TRUE)
        m <- matrix(as.numeric(l),nrow=length(l),ncol=1L,dimnames=list(d,'qd'))
        isNum <- !is.na(m[,1L])
        isNum <- (cumsum(isNum)>0) & rev(cumsum(rev(isNum))>0)
        m <- m[isNum,,drop=FALSE]
        if(any(diff(as.Date(rownames(m)))!=1L)) {
            warning('irregular series',immediate.=TRUE)
        }
        
    } else {
        
        m <- list(file=file,name=name,site=site,id=id,skip=skip)
        
    }

    if(id2!=id && as.integer(id2) != as.integer(id)) {
        warning('id does not match',immediate.=TRUE)
    }
    
    return(m)

}

.readDailySeriesLubw <- function(file=NULL,id=NULL,series=TRUE) {

    l <- readLines(file,encoding='latin1',n=ifelse(series,-1L,200L))
    n <- length(l)
    ll <- l[1:ifelse(n>200,200L,n)]

    site <- sub('^.*? (.*?)/.*$','\\1',ll[1])
    name <- sub('^.*?/(.*?) \\(.*$','\\1',ll[1])
    id2 <- sub('^([[:digit:]]+).*$','\\1',ll[1])
    skip <- 1L
    
    if(series) {
        
        l <- l[-(1:skip)]
        d <- sub('^([[:digit:]]{2})\\.([[:digit:]]{2})\\.([[:digit:]]{4}).*$','\\3-\\2-\\1',l)
        l <- sub('^.*[[:space:]]+','',l)
        l <- sub(',','.',l,fixed=TRUE)
        m <- matrix(as.numeric(l),nrow=length(l),ncol=1L,dimnames=list(d,'qd'))
        m[m[,1L]<0,1L] <- NA
        isNum <- !is.na(m[,1L])
        isNum <- (cumsum(isNum)>0) & rev(cumsum(rev(isNum))>0)
        m <- m[isNum,,drop=FALSE]
        if(any(diff(as.Date(rownames(m)))!=1)) {
            warning('irregular series',immediate.=TRUE)
        }
        
    } else {
        
        m <- list(file=file,name=name,site=site,id=id,skip=skip)
        
    }
    
    if(id2!=id && as.integer(id2) != as.integer(id)) {
        warning('id does not match',immediate.=TRUE)
    }
    
    return(m)

}

.readDailySeriesMapama <- function(file=NULL,id=NULL,series=TRUE) {

    l <- readLines(file,encoding='UTF-8')

    l <- gsub('<[^!].*?>','',l)
    l <- sub('<![CDATA[','',l,fixed=TRUE)
    l <- sub(']]>','',l,fixed=TRUE)
    l <- sub(',','.',l,fixed=TRUE)
    l <- sub('-[[:digit:]]+','',l)
    l <- l[grepl('^[[:digit:]-]+',l)]

    mt <- matrix(as.numeric(l),ncol=15L,nrow=length(l)/15,byrow=TRUE)
    site <- NA
    name <- NA
    id2 <- mt[1L,1L]
    skip <- NA

    if(series) {
        
        r <- range(mt[,2L])
        r <- as.Date(c(paste0(r[1L],'-01-01'),paste0(r[2L]+1L,'-12-31')))
        d <- seq.Date(r[1L],r[2L],by='day')
        m <- matrix(NA,nrow=length(d),ncol=1L,dimnames=list(as.character(d),'qd'))
        mi <- c('10','11','12','01','02','03','04','05','06','07','08','09')
        for(i in 1:12) {
            xx <- mt[,i+3L]
            names(xx) <- paste(mt[,2L]+ifelse(i<4,0L,1L),mi[i],
                               formatC(mt[,3L],width=2,flag='0'),sep='-')
            xx <- xx[names(xx)%in%as.character(d)]
            m[names(xx),1L] <- xx
        }
        isNum <- !is.na(m[,1L])
        isNum <- (cumsum(isNum)>0) & rev(cumsum(rev(isNum))>0)
        m <- m[isNum,,drop=FALSE]
        if(any(diff(as.Date(rownames(m)))!=1)) {
            warning('irregular series',immediate.=TRUE)
        }
	
    } else {
        
        m <- list(file=file,name=name,site=site,id=id,skip=skip)
        
    }

    if(id2!=id && as.integer(id2) != as.integer(id)) {
        warning('id does not match',immediate.=TRUE)
    }
    
    return(m)

}

.readDailySeriesMedde <- function(file=NULL,id=NULL,series=TRUE) {

    l <- readLines(file,encoding='latin1',n=ifelse(series,-1L,200L))
    n <- length(l)
    ll <- l[1:ifelse(n>200,200L,n)]

    i <- grep('^Code station',ll)[1L]
    h <- read.table(text=ll[i:(i+1L)],sep=';')
    site <- sub('^.*? \u00E0 (.*)$','\\1',h[2,2])
    name <- sub('^(.*?) \u00E0 .*$','\\1',h[2,2])
    id2 <- h[2,1]
    skip <- NA

    if(series) {

        i1 <- grepl('^Proc',l)
        i2 <- grepl('^[[:digit:]]+',l)
        l <- l[i1 | i2]

        yi <- which(grepl('^Proc',l))
        y <- as.numeric(gsub('[^[:digit:]]','',l[yi]))
        yr <- as.Date(paste0(range(y),c('-01-01','-12-31')))
        d <- seq.Date(yr[1],yr[2],by='day')

        m <- matrix(NA,nrow=length(d),ncol=1L,dimnames=list(as.character(d),'qd'))

        ds <-   paste(rep(formatC(1:12,width=2,flag='0'),each=31),
                      rep(formatC(1:31,width=2,flag='0'),times=12),sep='-')

        for(i in 1:length(y)) {

            lt <- l[(yi[i]+1):(yi[i]+31)]
            lt <- paste(lt,collapse='\n')

            lt <- read.table(text=lt,sep=';')
            lt <- as.matrix(lt)
            lt <- lt[,seq(from=2,to=24,by=2)]
            dim(lt) <- NULL
            names(lt) <- paste(rep(y[i],31*12),ds,sep='-')
            
            ri <- as.Date(paste0(c(y[i],y[i]),c('-01-01','-12-31')))
            di <- as.character(seq.Date(ri[1],ri[2],by='day'))
            m[di,'qd'] <- as.numeric(lt[di])
            
        }

        isNum <- !is.na(m[,1L])
        isNum <- (cumsum(isNum)>0) & rev(cumsum(rev(isNum))>0)
        m <- m[isNum,,drop=FALSE]
        if(any(diff(as.Date(rownames(m)))!=1L)) {
            warning('irregular series',immediate.=TRUE)
        }

    } else {
        
        m <- list(file=file,name=name,site=site,id=id,skip=skip)
        
    }

    if(id2!=id && as.integer(id2) != as.integer(id)) {
        warning('id does not match',immediate.=TRUE)
    }

    return(m)

}

.readDailySeriesMetsw <- function(file=NULL,id=NULL,series=TRUE,vars=NULL) {
    
    ff <- unzip(file,list=TRUE)
    ff <- ff$Name[grepl('^order_[0-9]+_data\\.txt$',ff$Name)]
    co <- unz(file,ff)
    l <- readLines(co)
    close(co)

    i <- range(grep(paste0('^',id),l))
    df <- read.table(text=l[(i[1L]-1L):i[2L]],header=TRUE,sep=';',
                     quote='',na.strings='-',blank.lines.skip=FALSE,
                     stringsAsFactors=FALSE)

    if(series) {
        
        id2 <- df[1L,'stn']
        i <- vars%in%names(df)
        if(!any(i)) {return(NULL)}
        vars <- vars[i]

        d <- as.Date(as.character(df$time),'%Y%m%d')
        m <- as.matrix(df[,vars])
        rownames(m) <- as.character(d)
        
        isNum <- apply(!is.na(m),1L,any)
        isNum <- (cumsum(isNum)>0) & rev(cumsum(rev(isNum))>0)
        m <- m[isNum,,drop=FALSE]
        if(any(diff(d[isNum])!=1L)) {
            warning('irregular series',immediate.=TRUE)
        }
        
    } else {
        
        m <- list(file=file,id=id,first.row=i[1L],last.row=i[2L])

    }

    if(id2!=id) {
        warning('id does not match',immediate.=TRUE)
    }
    
    return(m)
    
}

