.initFileSpecs <- function(files) {

    cn <- c('file','id','name','site','unit','type','start','end',
            'comment','hydropro','sep','skip')
    
    m <- matrix(NA,nrow=length(files),ncol=length(cn),dimnames=list(NULL,cn))
    m <- as.data.frame(m,stringsAsFactors=F,row.names=NULL)
    m$file <- files
    
    return(m)

}

.getFileHeadTail <- function(file.name,encoding) {

    fs <- file.info(file.name)$size
    co <- file(file.name,open='r',encoding=encoding)
    
    if(fs<2e4) {
        l <-  readLines(con=co)
    } else {
        l1 <- readLines(con=co,n=200)
        ln <- seek(co,where=fs-5000,origin='start')
        l2 <- readLines(con=co)
        l <- c(l1,l2[-1])
    }
    
    close(co)
    return(l)

}

## x must have standard POSIX format YYYY-MM-DD HH:MM
.getTimestamps <- function(x,time.res) {

    ## overwrite last digit of HH:MM (=HH:M0)
    ## doing so we truncate irregular series to regular 10min series
    x <- sub('[[:digit:]]{1}$','0',x)

    ## number of required characters
    d <- as.POSIXct(x,tz='UTC')
    nc <- 16
    nc <- .getTimestampLength(time.res)

    return(substr(x,1,nc))
    
}

.getRegularTimestamps <- function(start,end,time.res) {

    ext <- .getTimestampExtension(time.res)
    by <- .getTimestampInterval(time.res)
    nc <- .getTimestampLength(time.res)
    r <- as.POSIXct(paste0(c(start,end),ext),tz='UTC')
    x <- seq.POSIXt(from=r[1],to=r[2],by=by,tz='UTC')

    return(substr(x,1,nc))

}

.getTimestampLength <- function(time.res) {

    ## standard POSIX format YYYY-MM-DD HH:MM
    ## used to truncate to the temporal resolution 
    switch(time.res,
           year=4,
           month=7,
           day=10,
           hour=13,
           min=16,
           irregular=16)
    
}

.getTimestampExtension <- function(time.res) {

    ## standard POSIX format YYYY-MM-DD HH:MM
    ## used to extend a truncated time string
    switch(time.res,
           year='-01-01 00:00',
           month='-01 00:00',
           day=' 00:00',
           hour=':00',
           min='',
           irregular='')

}

.getTimestampInterval <- function(time.res) {

    ## used for seq.POSIXt
    switch(time.res,
           year='year',
           month='month',
           day='day',
           hour='hour',
           min='10 min',
           irregular='10 min')

    
}

## x: a representative line with observations
.getFieldSeparator <- function(x) {

    y <- ''
    
    if(grepl('\t',x,fixed=T)) {
        y <- '\t'
    }

    if(grepl(',',x,fixed=T)) {
        y <- ','
    }
    
    if(grepl(';',x,fixed=T)) {
        y <- ';'
    }

    return(y)

}

.getDuplicates <- function(x) {

    n <- length(x)
    l <- vector(mode='list',length=n)
    
    for(i in 1:n) {
        if(is.na(x[i])) {
            next
        } else {
            j <- which(x[i]==x)
            if(length(j)>1) {
                j <- setdiff(j,i)
                l[[i]] <- list(to=i,from=j)
                x[c(i,j)] <- NA
            }
        }
    }
    
    return(l)

}


