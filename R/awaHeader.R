awaHeader <-
function(h=NULL) {
  
  if(grepl('m[^[:blank:]]*?/s',h$var)) {h$var <- 'm3/s'}
  if(grepl('l/s',h$var)) {h$var <- 'l/s'}
  if(grepl('Wsfl',h$var)) {h$var <- 'm'}
  if(grepl('emperatur',h$var)) {h$var <- 'dC'}
  return(h)
  
}

awaHeaderHydropro <-
function(li=NULL) {
  
  h <- list(id=NA,name=NA,site=NA,var=NA,comment=NA,skip=NA)
  
  li <- gsub('Luecke|L\u00FCcke|L\\xfccke|NA|Lcke|L\u0081cke','',li)
  sk <- grep('^[[:blank:][:digit:][:cntrl:];,.:/-]+$',li)[1]-1
  li <- li[1:sk]
  li <- gsub('([*%";[:cntrl:]])','',li)
  li <- sub('Datum.*','',li)
  li <- sub('ASCII.*','',li)
  li <- li[grepl('[[:alnum:]]',li)]
  
  i <- grep('^HBBE',li)
  h$id <- sub('^.*?-([AGP]{1}[0123456789]+)-.*','\\1',li[i])
  li <- sub(h$id,'',li,fixed=ifelse(nchar(h$id)>0,T,F))
  li <- sub('^HBBE[[:alnum:]]?--[[:digit:]]+-[[:digit:]]+$','',li)
  
  i <- grep('Abfl|Wsfl',li)
  if(length(i)==1) {
    h$var <- sub('^.*?(Abfl.*$|Wsfl.*$)','\\1',li[i])
    li <- sub(h$var,'',li,fixed=ifelse(nchar(h$var)>0,T,F))
  }
  
  i <- grep('^[[:alpha:][:blank:][:punct:]]+,[[:alpha:][:blank:][:punct:]]+$',li)
  if(length(i) == 1L) {
    h$name <- sub('^([[:alpha:][:blank:][:punct:]]+),.*$','\\1',li[i])
    h$site <- sub('^[[:alpha:][:blank:][:punct:]]+,([^-]+).*$','\\1',li[i])
    li <- sub(h$name,'',li,fixed=ifelse(nchar(h$name)>0,T,F))
    li <- sub(h$site,'',li,fixed=ifelse(nchar(h$site)>0,T,F))
  }
  
  li <- gsub('^[[:blank:][:punct:][:cntrl:]]+','',li)
  li <- gsub('[[:blank:][:punct:][:cntrl:]]+$','',li)
  li <- li[grepl('[[:alnum:]]',li)]
  h$comment <- paste(li,collapse=' // ')
  h <- lapply(h,function(x){sub('^[[:blank:][:cntrl:];,.:/-]+','',x)})
  h <- lapply(h,function(x){sub('[[:blank:][:cntrl:];,.:/-]+$','',x)})
  h$skip <- sk
  
  h <- awaHeader(h)
  return(h)
  
} 
