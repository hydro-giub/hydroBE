fetchBafuTable <- function(id,year,param=c('Q','P')[1]) {

    f <- paste0('http://www.hydrodaten.admin.ch/lhg/sdi/jahrestabellen/',
                id,param,'_',substr(year,3,4),'.pdf')
    l <- readLines(f,warn=FALSE,encoding='latin1')
    l <- l[grepl('^T\\*(.*?)Tj$',l)]
    l <- sub('T* (','',l,fixed=TRUE)
    l <- sub(') Tj','',l,fixed=TRUE)
    l <- l[!grepl('^[-+]+$',l)]
    
    p <- any(grepl('Provisorische Daten',l))
    d <- seq.Date(from=as.Date(paste(year,'01','01',sep='-')),
                  to=as.Date(paste(year,'12','31',sep='-')),by='day')
    df <- data.frame(date=d,discharge=NA,preliminary=p)
    n <- table(format(df$date,format='%m'))
    mi <- c(1,(cumsum(n)+1)[-12])
    mn <- c('Genn.','Febbr.','Marzo','Aprile','Maggio','Giugno',
            'Luglio','Agosto','Sett.','Ott.','Nov.','Dic.')

    for(i in 1:12) {

        j <- grep(mn[i],l,fixed=TRUE)+1
        nj <- length(j)

        if(nj<1) {next}
        if(nj>1) {j <- j[nj]}

        jj <- 0
        while (grepl('^[[:digit:]. ]+$',l[j+jj])) {jj <- jj+1}

        if (jj>=n[i]) {
            df$discharge[mi[i]:(mi[i]+n[i]-1)] <- as.numeric(l[j:(j+n[i]-1)])
        }

        if (jj<n[i] & jj>0) {
            df$discharge[mi[i]:(mi[i]+jj-1)] <- as.numeric(l[j:(j+jj-1)])
        }
        
    }
    isNum <- !is.na(df$discharge) 
    isNum <- (cumsum(isNum)>0) & rev(cumsum(rev(isNum))>0)
    if(param=='P') {names(df)[2]<-'waterlevel'}
    return(df[isNum,])
    
}
