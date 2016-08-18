importDailySeriesBafu <- function(file=NULL,id=NULL,series=T,quiet=T) {
    
    p <- dirname(file)
    f <- basename(file)
    file <- list.files(path=p,pattern=f,full.names=T)
    if (length(file)!=1) {
        warning('file not found or multiple matches')
        return(NULL)
    }
    
    l <- readLines(file,encoding='latin1',n=ifelse(series,-1L,200))
    n <- length(l)
    ll <- l[1:ifelse(n>200,200,n)]
    
    h <- ll[grepl(paste('^[[:digit:]]+ / ',id,'.*$',sep=''),ll)]
    site <- sub('.*?-(.*) [[:digit:]]+.*','\\1',h)
    name <- sub('.*[[:digit:]]+ (.*?)-.* [[:digit:]]+.*','\\1',h)
    id2 <- sub('.*/ ([[:digit:]]+) .*','\\1',h)
    skip <- grep(paste(id,';[-[:digit:];,. ]+$',sep=''),ll)[1]-1

    if(series) {
        l <- l[-(1:skip)]
        l <- sub('^[[:digit:]]+;','',l)
        d <- sub('^([[:digit:]]{4}\\.[[:digit:]]{2}\\.[[:digit:]]{2}).*$','\\1',l)
        d <- gsub('.','-',d,fixed=T)
        l <- sub('^.*;(.*)$','\\1',l)
        m <- matrix(as.numeric(l),nrow=length(l),ncol=1,dimnames=list(d,'qd'))
        isNum <- !is.na(m[,1])
        isNum <- (cumsum(isNum)>0) & rev(cumsum(rev(isNum))>0)
        m <- m[isNum,,drop=F]
        if(any(diff(as.Date(rownames(m)))!=1)) {warning('irregular series',immediate.=T)}
    } else {
        m <- list(file=file,name=name,site=site,id=id,skip=skip)
    }
    
    if(id2!=id & as.integer(id2) != as.integer(id)) {warning('id does not match',immediate.=T)}
    if(!quiet) {print(paste0('file: ',file,'; site: ',site,'; name: ',name,'; id: ',id2))}

    return(m)
    
}

importDailySeriesBmlfuw <- function(file=NULL,id=NULL,series=T,quiet=T) {

    p <- dirname(file)
    f <- basename(file)
    file <- list.files(path=p,pattern=f,full.names=T)
    if (length(file)!=1) {
        warning('file not found or multiple matches')
        return(NULL)
    }

    l <- readLines(file,encoding='latin1',n=ifelse(series,-1L,200))
    n <- length(l)
    ll <- l[1:ifelse(n>200,200,n)]
    
    site <- ll[grepl('^Messstelle: *;.*$',ll)]
    site <- sub('^Messstelle: *;(.*)$','\\1',site)
    name <- ll[grepl('^Gew\u00E4sser: *;.*$',ll)]
    name <- sub('^Gew\u00E4sser: *;(.*)$','\\1',name)
    id2 <- ll[grepl('^HZB-Nummer: *;.*$',ll)]
    id2 <- sub('^HZB-Nummer: *;(.*)$','\\1',id2)
    skip <- grep('^Werte:$',ll)[1]

    if(series) {
        l <- l[-(1:skip)]
        d <- sub('^([[:digit:]]{2})\\.([[:digit:]]{2})\\.([[:digit:]]{4}).*$','\\3-\\2-\\1',l)
        l <- sub('^.*;(.*)$','\\1',l)
        m <- matrix(as.numeric(l),nrow=length(l),ncol=1,dimnames=list(d,'qd'))
        isNum <- !is.na(m[,1])
        isNum <- (cumsum(isNum)>0) & rev(cumsum(rev(isNum))>0)
        m <- m[isNum,,drop=F]
        if(any(diff(as.Date(rownames(m)))!=1)) {warning('irregular series',immediate.=T)}
    } else {
        m <- list(file=file,name=name,site=site,id=id,skip=skip)
    }

    if(id2!=id & as.integer(id2) != as.integer(id)) {warning('id does not match',immediate.=T)}
    if(!quiet) {print(paste0('file: ',file,'; site: ',site,'; name: ',name,'; id: ',id2))}

    return(m)

}

importDailySeriesGrdc <- function(file=NULL,id=NULL,series=T,quiet=T) {

    p <- dirname(file)
    f <- basename(file)
    file <- list.files(path=p,pattern=f,full.names=T)
    if (length(file)!=1) {
        warning('file not found or multiple matches')
        return(NULL)
    }
    
    l <- readLines(file,encoding='latin1',n=ifelse(series,-1L,200))
    n <- length(l)
    ll <- l[1:ifelse(n>200,200,n)]

    site <- ll[grep('Station:',ll,fixed=T)]
    site <- sub('^.*?Station:[[:blank:]]*(.*)$','\\1',site)
    name <- ll[grep('River:',ll,fixed=T)]
    name <- sub('^.*?River:[[:blank:]]*(.*)$','\\1',name)
    id2 <- ll[grep('GRDC-No.:',ll,fixed=T)]
    id2 <- sub('^.*?GRDC-No\\.:[[:blank:]]*(.*)$','\\1',id2)
    skip <- grep('^[[:digit:]]+[[:digit:][:punct:][:blank:]]+$',ll)[1]-1

    if(series) {
        l <- l[-(1:skip)]
        d <- sub('^([[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}).*$','\\1',l)
        l <- sub(';[^;]*$','',l)
        l <- sub('^.*;(.*)$','\\1',l)
        m <- matrix(as.numeric(l),nrow=length(l),ncol=1,dimnames=list(d,'qd'))
        m[m[,1]<0,1] <- NA
        isNum <- !is.na(m[,1])
        isNum <- (cumsum(isNum)>0) & rev(cumsum(rev(isNum))>0)
        m <- m[isNum,,drop=F]
        if(any(diff(as.Date(rownames(m)))!=1)) {warning('irregular series',immediate.=T)}
    } else {
        m <- list(file=file,name=name,site=site,id=id,skip=skip)
    }

    if(id2!=id & as.integer(id2) != as.integer(id)) {warning('id does not match',immediate.=T)}
    if(!quiet) {print(paste0('file: ',file,'; site: ',site,'; name: ',name,'; id: ',id2))}

    return(m)

}

importDailySeriesLfub <- function(file=NULL,id=NULL,series=T,quiet=T) {

    p <- dirname(file)
    f <- basename(file)
    file <- list.files(path=p,pattern=f,full.names=T)
    if (length(file)!=1) {
        warning('file not found or multiple matches')
        return(NULL)
    }

    l <- readLines(file,encoding='latin1',n=ifelse(series,-1L,200))
    n <- length(l)
    ll <- l[1:ifelse(n>200,200,n)]

    site <- ll[grepl('^Messstellen-Name[:;\"]*.*?[.;\"]*$',ll)]
    site <- sub('^Messstellen-Name[:;\"]*(.*?)[.;\"]*$','\\1',site)
    name <- ll[grepl('^Gew\u00E4sser[:;\"]*.*?[:;\"]*$',ll)]
    name <- sub('^Gew\u00E4sser[:;\"]*(.*?)[:;\"]*$','\\1',name)
    id2 <- ll[grepl('^Messstellen-Nr[.:;\"]*[0-9]+$',ll)]
    id2 <- sub('^Messstellen-Nr[.:;\"]*([0-9]+)$','\\1',id2)
    skip <- grep('^Datum;',ll)[1]

    if(series) {
        l <- l[-(1:skip)]
        d <- sub('^([[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}).*$','\\1',l)
        l <- sub('^[-[:digit:]]+;([[:digit:],]+);.*$','\\1',l)
        l <- sub('^.*;(.*)$','\\1',l)
        l <- sub(',','.',l,fixed=T)
        m <- matrix(as.numeric(l),nrow=length(l),ncol=1,dimnames=list(d,'qd'))
        isNum <- !is.na(m[,1])
        isNum <- (cumsum(isNum)>0) & rev(cumsum(rev(isNum))>0)
        m <- m[isNum,,drop=F]
        if(any(diff(as.Date(rownames(m)))!=1)) {warning('irregular series',immediate.=T)}
    } else {
        m <- list(file=file,name=name,site=site,id=id,skip=skip)
    }

    if(id2!=id & as.integer(id2) != as.integer(id)) {warning('id does not match',immediate.=T)}
    if(!quiet) {print(paste0('file: ',file,'; site: ',site,'; name: ',name,'; id: ',id2))}

    return(m)

}

importDailySeriesLubw <- function(file=NULL,id=NULL,series=T,quiet=T) {

    p <- dirname(file)
    f <- basename(file)
    file <- list.files(path=p,pattern=f,full.names=T)
    if (length(file)!=1) {
        warning('file not found or multiple matches')
        return(NULL)
    }

    l <- readLines(file,encoding='latin1',n=ifelse(series,-1L,200))
    n <- length(l)
    ll <- l[1:ifelse(n>200,200,n)]

    site <- sub('^.*? (.*?)/.*$','\\1',ll[1])
    name <- sub('^.*?/(.*?) \\(.*$','\\1',ll[1])
    id2 <- sub('^([[:digit:]]+).*$','\\1',ll[1])
    skip <- 1
    
    if(series) {
        l <- l[-(1:skip)]
        d <- sub('^([[:digit:]]{2})\\.([[:digit:]]{2})\\.([[:digit:]]{4}).*$','\\3-\\2-\\1',l)
        l <- sub('^.*[[:space:]]+','',l)
        l <- sub(',','.',l,fixed=T)
        m <- matrix(as.numeric(l),nrow=length(l),ncol=1,dimnames=list(d,'qd'))
        m[m[,1]<0,1] <- NA
        isNum <- !is.na(m[,1])
        isNum <- (cumsum(isNum)>0) & rev(cumsum(rev(isNum))>0)
        m <- m[isNum,,drop=F]
        if(any(diff(as.Date(rownames(m)))!=1)) {warning('irregular series',immediate.=T)}
    } else {
        m <- list(file=file,name=name,site=site,id=id,skip=skip)
    }
  
    if(id2!=id & as.integer(id2) != as.integer(id)) {warning('id does not match',immediate.=T)}
    if(!quiet) {print(paste0('file: ',file,'; site: ',site,'; name: ',name,'; id: ',id2))}

    return(m)

}
