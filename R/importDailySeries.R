importDailySeriesBafu <- function(file=NULL,id=NULL,series=TRUE,quiet=TRUE) {
    
    p <- dirname(file)
    f <- basename(file)
    file <- list.files(path=p,pattern=f,full.names=TRUE)
    if(length(file)!=1L) {
        warning('file not found or multiple matches')
        return(NULL)
    }
    
    l <- readLines(file,encoding='latin1',n=ifelse(series,-1L,200L))
    n <- length(l)
    ll <- l[1:ifelse(n>200,200L,n)]
    
    h <- ll[grepl(paste('^[[:digit:]]+ / ',id,'.*$',sep=''),ll)]
    site <- sub('.*?-(.*) [[:digit:]]+.*','\\1',h)
    name <- sub('.*[[:digit:]]+ (.*?)-.* [[:digit:]]+.*','\\1',h)
    id2 <- sub('.*/ ([[:digit:]]+) .*','\\1',h)
    skip <- grep(paste(id,';[-[:digit:];,. ]+$',sep=''),ll)[1L]-1L

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
    
    if(!quiet) {
        print(paste0('file: ',file,'; site: ',site,'; name: ',name,'; id: ',id2))
    }

    return(m)
    
}

importDailySeriesBmlfuw <- function(file=NULL,id=NULL,series=TRUE,quiet=TRUE) {

    p <- dirname(file)
    f <- basename(file)
    file <- list.files(path=p,pattern=f,full.names=TRUE)
    if(length(file)!=1L) {
        warning('file not found or multiple matches')
        return(NULL)
    }

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
    
    if(!quiet) {
        print(paste0('file: ',file,'; site: ',site,'; name: ',name,'; id: ',id2))
    }

    return(m)

}

importDailySeriesGrdc <- function(file=NULL,id=NULL,series=TRUE,quiet=TRUE) {

    p <- dirname(file)
    f <- basename(file)
    file <- list.files(path=p,pattern=f,full.names=TRUE)
    if(length(file)!=1L) {
        warning('file not found or multiple matches')
        return(NULL)
    }
    
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
    
    if(!quiet) {
        print(paste0('file: ',file,'; site: ',site,'; name: ',name,'; id: ',id2))
    }

    return(m)

}

importDailySeriesLfub <- function(file=NULL,id=NULL,series=TRUE,quiet=TRUE) {

    p <- dirname(file)
    f <- basename(file)
    file <- list.files(path=p,pattern=f,full.names=TRUE)
    if(length(file)!=1L) {
        warning('file not found or multiple matches')
        return(NULL)
    }

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
    
    if(!quiet) {
        print(paste0('file: ',file,'; site: ',site,'; name: ',name,'; id: ',id2))
    }

    return(m)

}

importDailySeriesLubw <- function(file=NULL,id=NULL,series=TRUE,quiet=TRUE) {

    p <- dirname(file)
    f <- basename(file)
    file <- list.files(path=p,pattern=f,full.names=TRUE)
    if(length(file)!=1L) {
        warning('file not found or multiple matches')
        return(NULL)
    }

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
    
    if(!quiet) {
        print(paste0('file: ',file,'; site: ',site,'; name: ',name,'; id: ',id2))
    }

    return(m)

}

importDailySeriesMapama <- function(file=NULL,id=NULL,series=TRUE,quiet=TRUE) {

    p <- dirname(file)
    f <- basename(file)
    file <- list.files(path=p,pattern=f,full.names=TRUE)
    if(length(file)!=1L) {
        warning('file not found or multiple matches')
        return(NULL)
    }

    l <- readLines(file,encoding='UTF-8')

    l <- gsub('<[^!].*?>','',l)
    l <- sub('<![CDATA[','',l,fixed=T)
    l <- sub(']]>','',l,fixed=T)
    l <- sub(',','.',l,fixed=T)
    l <- sub('-[[:digit:]]+','',l)
    l <- l[grepl('^[[:digit:]-]+',l)]

    mt <- matrix(as.numeric(l),ncol=15L,nrow=length(l)/15,byrow=T)
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
    
    if(!quiet) {
        print(paste0('file: ',file,'; site: ',site,'; name: ',name,'; id: ',id2))
    }

    return(m)

}

importDailySeriesMs <- function(file=NULL,id=NULL,series=TRUE,quiet=TRUE,vars=NULL,cnames=NULL) {
    
    p <- dirname(file)
    f <- basename(file)
    file <- list.files(path=p,pattern=f,full.names=TRUE)
    if(length(file)!=1L) {
        warning('file not found or multiple matches')
        return(NULL)
    }
    
    ff <- unzip(file,list=TRUE)
    ff <- ff$Name[grepl('^order_[0-9]+_data\\.txt$',ff$Name)]
    c <- unz(file,ff)
    l <- readLines(c)
    close(c)
    i <- range(grep(paste('^',id,sep=''),l))
    df <- read.table(text=l[(i[1L]-1L):i[2L]],header=TRUE,sep=';',quote='',na.strings='-',
                     blank.lines.skip=FALSE,stringsAsFactors=FALSE)

    if(series) {
        id2 <- df[1L,'stn']
        i <- vars%in%names(df)
        if(!any(i)) {return(NULL)}
        vars <- vars[i]
        if(is.null(cnames)) {cnames <- vars} else {cnames <- cnames[i]}
        m <- matrix(df[,vars],nrow=nrow(df),ncol=length(vars),
                    dimnames=list(as.character(as.Date(as.character(df$time),'%Y%m%d')),cnames))
        isNum <- apply(!is.na(m),1L,any)
        isNum <- (cumsum(isNum)>0) & rev(cumsum(rev(isNum))>0)
        m <- m[isNum,,drop=FALSE]
        if(any(diff(as.Date(rownames(m)))!=1L)) {
            warning('irregular series',immediate.=TRUE)
        }
    } else {
        m <- list(file=file,id=id,first.row=i[1L],last.row=i[2L])
    }
    
    if(id2!=id) {
        warning('id does not match',immediate.=TRUE)
    }
    
    if(!quiet) {
        print(paste0('file: ',file,'; id: ',id2))
    }
    
    return(m)
    
}

