readFilesBafu <- function(dir=NULL,files,time.res,series=FALSE,merge) {

    if(!is.null(dir)) {
        files <- list.files(dir,full.names=T)
    }

    file.specs <- .initFileSpecs(files=files)

    for(i in 1:length(files)) {
        h <- .checkFileBafu(file.name=files[i],time.res=time.res)
        file.specs[i,names(h)] <- unlist(h)
    }

    if(series) {
        return(.getSeriesBafu(file.specs=file.specs,
                              time.res=time.res,
                              merge=merge))
    } else {
        return(file.specs)
    }

}

.getSeriesBafu <- function(file.specs,time.res,merge) {
    
    n <- nrow(file.specs)
    
    ## ceate unique names out of the ids
    cn <- paste0('i',1:n,'i',file.specs$id)

    ## make regular timestamps
    i1 <- which.min(as.numeric(gsub('[-: ]','',file.specs$start)))
    i2 <- which.max(as.numeric(gsub('[-: ]','',file.specs$end)))
    rn <- .getRegularTimestamps(start=file.specs$start[i1],
                                end=file.specs$end[i2],
                                time.res=time.res)

    ## init matrix
    m <- matrix(NA,nrow=length(rn),ncol=n,
                dimnames=list(as.character(rn),cn))
    hp <- file.specs$hydropro
    sk <- file.specs$skip

    ## read series
    for(i in 1:nrow(file.specs)) {
    
        co <- file(file.specs$file[i],open='r',encoding='ISO-8859-1')
        x <-  readLines(con=co)
        close(co)
        
        if(sk[i]>0) {
            x <- x[-(1:sk[i])]
        }

        if(hp[i]) {
            x <- .cleanSeriesBafuHydropro(x=x,sep=file.specs$sep[i])
        } else {
            x <- .cleanSeriesBafuRegular(x=x,sep=file.specs$sep[i])
        }

        m[.getTimestamps(names(x),time.res=time.res),i] <- x

    }

    ## merge duplicated series
    ## missing attributes result in ..
    ## we do not merge such stations
    if(!missing(merge)) {
        
        id <- as.matrix(file.specs[,merge])
        id <- apply(id,1,paste,collapse='.')
        nr <- grepl('^\\.*$',id) | grepl('\\.\\.',id)
        dp <- .getDuplicates(id)
        dp[nr] <- NULL
        rmc <- unlist(lapply(dp,'[','from'),use.names=F)
        
        if(length(dp)>0 & !is.null(rmc)) {

            for(i in 1:length(dp)) {
                if(is.null(dp[[i]])) {
                    next
                } else {
                    rpl <- dp[[i]][['from']]
                    trg <- dp[[i]][['to']]
                    for(j in rpl) {
                        na <- is.na(m[,trg])
                        m[na,trg] <- m[na,j]
                    }           
                }       
            }

            return(m[,-rmc,drop=F])
            
        }
        
    } else {

        return(m)
        
    }
    
}

.checkFileBafu <- function(file.name,time.res) {

    ## Bafu uses the ISO-8859-1 character encoding
    ## files exported from the hydropro software differ in the header format
    x <- .getFileHeadTail(file.name=file.name,encoding='ISO-8859-1')
    hp <- any(grepl('[hH]ydro|[hH][bB][cC][hH]',x))

    if(hp) {
        
        h <- .checkBafuHeaderHydropro(x)

        l1 <- x[h$skip+1]

        h$sep <- .getFieldSeparator(l1)

        ## peak vs. mean values
        h$type <- 'unknown'

        ## remove header lines
        if(h$skip>0) {
            x <- x[-(1:h$skip)]
        }

        x <- .cleanSeriesBafuHydropro(x=x,sep=h$sep)
        
    } else {
        
        h <- .checkBafuHeaderRegular(x)

        l1 <- x[h$skip+1]

        ## field separator
        h$sep <- .getFieldSeparator(l1)

        ## peak vs. mean values
        if(grepl('-',l1)) {
            h$type <- 'mean'
        } else {
            h$type <- 'peak'
        }

        ## remove header lines
        if(h$skip>0) {
            x <- x[-(1:h$skip)]
        }

        x <- .cleanSeriesBafuRegular(x=x,sep=h$sep)
        
    }

    h$hydropro <- hp
    x <- .getTimestamps(names(x),time.res=time.res)
    h$start <- x[1]
    h$end <- x[length(x)]
    
    return(h)

}

## constrain the lines to format YYYY.MM.DD HH:MM;value
.cleanSeriesBafuHydropro <- function(x,sep) {

    ## adapt field separator
    x <- gsub(sep,';',x,fixed=T)

    ## remove all white space
    x <- gsub('[[:blank:]]','',x)

    ## if clock time is present, replace field delimiter with white space
    if(grepl(';.*?;',x[1])) {
        x <- sub(';',' ',x,fixed=T)
    }
    
    ## attach clock time and constrain to HH:MM
    x <- sub(';',' 00:00;',x,fixed=T)
    x <- sub('^([[:digit:].]{10}) ([[:digit:]]{2}:[[:digit:]]{2}).*?;(.*)$','\\1 \\2;\\3',x)

    ## constrain DD.MM.YYYY to YYYY.MM.DD
    x <- sub('^([[:digit:]]{2})\\.([[:digit:]]{2})\\.([[:digit:]]{4})(.*)$','\\3.\\2.\\1\\4',x)

    ## set 24:xx to 00:xx
    ## needs also to adjust the day
    i <- grepl('24:00',x,fixed=T)
    if(any(i)) {
        xd <- sub(';.*$','',x[i])
        xd <- as.POSIXct(xd,format='%Y.%m.%d %H:%M',tz='UTC')
        xd <- format(xd,'%Y.%m.%d %H:%M',tz='UTC')
        xv <- sub('^.*?;','',x[i])
        x[i] <- paste(xd,xv,sep=';')
    }
    
    ## set missing values to NA
    i1 <- grepl(';\\.?[[:digit:]]+\\.?[[:digit:]]*$',x)
    x[!i1] <- sub(';.*$',';NA',x[!i1])
    
    ## remove leading and trailing NAs
    i2 <- (cumsum(i1)>0) & rev(cumsum(rev(i1))>0)
    x <- x[i2]
    
    ##  return as numeric vector and format date to YYYY-MM-DD HH:MM
    y <- as.numeric(sub('^.*?;','',x))
    yn <- sub(';.*$','',x)
    yn <- gsub('.','-',yn,fixed=T)
    names(y) <- yn
    return(y)
    
}

## constrain the lines to format YYYY.MM.DD HH:MM;value
.cleanSeriesBafuRegular <- function(x,sep) {

    ## remove all white space
    x <- gsub('[[:blank:]]','',x)
    
    ## remove first column (gauge id)
    x <- sub('^[[:digit:]]*?;','',x)
    
    ## remove time interval if present
    if(grepl('-',x[1])) {
        x <- sub('-[[:digit:][:blank:].:]+;',';',x)
    }
    
    ## expand YYYY.MM to YYYY.MM.DD
    if(grepl('^[[:digit:].]{7};',x[1])) {
        x <- sub(';','.01;',x,fixed=T)
    }

    ## attach clock time and constrain to HH:MM
    x <- sub(';','00:00;',x,fixed=T)
    x <- sub('^([[:digit:].]{10})([[:digit:]]{2}:[[:digit:]]{2}).*?;(.*)$','\\1 \\2;\\3',x)
    
    ## set missing values to NA
    i1 <- grepl(';\\.?[[:digit:]]+\\.?[[:digit:]]*$',x)
    x[!i1] <- sub(';.*$',';NA',x[!i1])
    
    ## remove leading and trailing NAs
    i2 <- (cumsum(i1)>0) & rev(cumsum(rev(i1))>0)
    x <- x[i2]

    ## return as numeric vector and format date to YYYY-MM-DD HH:MM
    y <- as.numeric(sub('^.*?;','',x))
    yn <- sub(';.*$','',x)
    yn <- gsub('.','-',yn,fixed=T)
    names(y) <- yn
    return(y)

}

.prettifyBafuHeader <- function(h) {

    ## prettify output from checkBafuHeaderRegular and checkBafuHeaderHydropro
    if(grepl('m[^[:blank:]]*?/s',h$unit)) {h$unit <- 'm3/s'}
    if(grepl('l/s',h$unit)) {h$unit <- 'l/s'}
    if(grepl('asserstand',h$unit)) {h$unit <- 'm'}
    if(grepl('emperatur',h$unit)) {h$unit <- 'dC'}
    h$id <- sub('^.*/[[:blank:]]*','',h$id)
    return(h)

}

.checkBafuHeaderHydropro <- function(x) {
    
    h <- list(id=NA,name=NA,site=NA,unit=NA,comment=NA,skip=NA)
    
    x <- gsub(.getNaRegex(),'',x)
    sk <- grep('^[[:blank:][:digit:][:cntrl:];,.:/-]+$',x)[1]-1
    if(sk<1 | length(sk)<1) {
        h$skip <- 0
        return(h)
    }
        
    x <- x[1:sk]
    x <- gsub('([*%";[:cntrl:]])','',x)
    x <- sub(' /[ dD]+onn.*?onal data','',x)
    x <- gsub('Datum|Zeit|Messwert','',x)
    x <- x[grepl('[[:alnum:]]',x)]
    
    i <- grep('^HBCH',x)
    h$id <- sub('HBCH[[:alnum:]]?-([[:digit:]]+)-.*','\\1',x[i])
    x <- sub(h$id,'',x,fixed=ifelse(nchar(h$id)>0,T,F))
    x <- sub('^HBCH[[:alnum:]]?--[[:digit:]]+-[[:digit:]]+$','',x)
    
    i <- grep('Abfluss|Wasserstand|Wassertemperatur',x)
    h$unit <- sub('^.*?(Abfluss.*$|Wasserstand.*$|Wassertemperatur.*$)','\\1',x[i])
    x <- sub(h$unit,'',x,fixed=ifelse(nchar(h$unit)>0,T,F))
    
    i <- grep('^[[:alpha:][:blank:]]+,[[:alpha:][:blank:]]+-$',x)
    if(length(i) == 1L) {
        h$name <- sub('^([[:alpha:][:blank:]]+),[[:alpha:][:blank:]]+-$','\\1',x[i])
        h$site <- sub('^[[:alpha:][:blank:]]+,([[:alpha:][:blank:]]+)-$','\\1',x[i])
        x <- sub(h$name,'',x,fixed=ifelse(nchar(h$name)>0,T,F))
        x <- sub(h$site,'',x,fixed=ifelse(nchar(h$site)>0,T,F))
    }
    
    x <- gsub('^[[:blank:][:punct:][:cntrl:]]+','',x)
    x <- gsub('[[:blank:][:punct:][:cntrl:]]+$','',x)
    x <- x[grepl('[[:alnum:]]',x)]
    h$comment <- paste(x,collapse=' // ')
    h <- lapply(h,function(x){sub('^[[:blank:][:cntrl:];,.:/-]+','',x)})
    h <- lapply(h,function(x){sub('[[:blank:][:cntrl:];,.:/-]+$','',x)})
    h$skip <- sk
    
    h <- .prettifyBafuHeader(h)
    return(h)
    
}

.checkBafuHeaderRegular <- function(x) {
    
    h <- list(id=NA,name=NA,site=NA,unit=NA,comment=NA,skip=NA)
    
    x <- gsub(.getNaRegex(),'',x)
    sk <- grep('^[[:blank:][:digit:][:cntrl:];,.:/-]+$',x)[1]-1
    if(sk<1 | length(sk)<1) {
        h$skip <- 0
        return(h)
    }
    
    x <- x[1:sk]
    x <- gsub('[*%";[:cntrl:]]','',x)
    x <- sub(' /[ dD]+onn.*?onal data','',x)
    x <- x[grepl('[[:alnum:]]',x)]
    
    i <- grepl('^[[:digit:][:blank:]]+/[[:digit:][:blank:]]+',x)
    h$id <- sub('^([[:digit:][:blank:][:punct:]]+).*$','\\1',x[i])
    h$name <- sub('^[[:digit:][:blank:]/]+(.*?)-.*$','\\1',x[i])
    h$site <- sub('^.*?(-.*?)[[:blank:][:digit:],.:;-]+$','\\1',x[i])
    x <- sub(h$id,'',x,fixed=ifelse(nchar(h$id)>0,T,F))
    x <- sub(h$name,'',x,fixed=ifelse(nchar(h$name)>0,T,F))
    x <- sub(h$site,'',x,fixed=ifelse(nchar(h$site)>0,T,F))
    
    i <- grep('Abfluss|Wasserstand|Wassertemperatur',x)
    h$unit <- sub('^.*?(Abfluss.*$|Wasserstand.*$|Wassertemperatur.*$)','\\1',x[i])
    x <- sub(h$unit,'',x,fixed=ifelse(nchar(h$unit)>0,T,F))
    
    x <- gsub('^[[:blank:][:punct:][:cntrl:]]+','',x)
    x <- gsub('[[:blank:][:punct:][:cntrl:]]+$','',x)
    x <- x[grepl('[[:alnum:]]',x)]
    h$comment <- paste(x,collapse=' // ')
    h <- lapply(h,function(x){sub('^[[:blank:][:cntrl:];,.:/-]+','',x)})
    h <- lapply(h,function(x){sub('[[:blank:][:cntrl:];,.:/-]+$','',x)})
    h$skip <- sk
    
    h <- .prettifyBafuHeader(h)
    return(h)
    
}
