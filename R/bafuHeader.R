bafuHeader <-
function(h=NULL) {
  
  if(grepl('m[^[:blank:]]*?/s',h$var)) {h$var <- 'm3/s'}
  if(grepl('l/s',h$var)) {h$var <- 'l/s'}
  if(grepl('asserstand',h$var)) {h$var <- 'm'}
  if(grepl('emperatur',h$var)) {h$var <- 'dC'}
  return(h)

}

bafuHeaderTxt <-
function(li=NULL) {
  
  h <- list(id=NA,name=NA,site=NA,var=NA,comment=NA,skip=NA)
  
  li <- gsub('Luecke|L\u00FCcke|L\\xfccke|NA|Lcke|L\u0081cke','',li)
  sk <- grep('^[[:blank:][:digit:][:cntrl:];,.:/-]+$',li)[1]-1
  li <- li[1:sk]
  li <- gsub('[*%";[:cntrl:]]','',li)
  li <- sub(' /[ dD]+onn.*?onal data','',li)
  li <- li[grepl('[[:alnum:]]',li)]
  
  i <- grepl('^[[:digit:][:blank:]]+/[[:digit:][:blank:]]+',li)
  h$id <- sub('^([[:digit:][:blank:][:punct:]]+).*$','\\1',li[i])
  h$name <- sub('^[[:digit:][:blank:]/]+(.*?)-.*$','\\1',li[i])
  h$site <- sub('^.*?(-.*?)[[:blank:][:digit:],.:;-]+$','\\1',li[i])
  li <- sub(h$id,'',li,fixed=ifelse(nchar(h$id)>0,T,F))
  li <- sub(h$name,'',li,fixed=ifelse(nchar(h$name)>0,T,F))
  li <- sub(h$site,'',li,fixed=ifelse(nchar(h$site)>0,T,F))
  
  i <- grep('Abfluss|Wasserstand|Wassertemperatur',li)
  h$var <- sub('^.*?(Abfluss.*$|Wasserstand.*$|Wassertemperatur.*$)','\\1',li[i])
  li <- sub(h$var,'',li,fixed=ifelse(nchar(h$var)>0,T,F))
  
  li <- gsub('^[[:blank:][:punct:][:cntrl:]]+','',li)
  li <- gsub('[[:blank:][:punct:][:cntrl:]]+$','',li)
  li <- li[grepl('[[:alnum:]]',li)]
  h$comment <- paste(li,collapse=' // ')
  h <- lapply(h,function(x){sub('^[[:blank:][:cntrl:];,.:/-]+','',x)})
  h <- lapply(h,function(x){sub('[[:blank:][:cntrl:];,.:/-]+$','',x)})
  h$skip <- sk
  
  h <- bafuHeader(h)
  return(h)
  
}

bafuHeaderHydropro <-
function(li=NULL) {
  
  h <- list(id=NA,name=NA,site=NA,var=NA,comment=NA,skip=NA)
  
  li <- gsub('Luecke|L\u00FCcke|L\\xfccke|NA|Lcke|L\u0081cke','',li)
  sk <- grep('^[[:blank:][:digit:][:cntrl:];,.:/-]+$',li)[1]-1
  li <- li[1:sk]
  li <- gsub('([*%";[:cntrl:]])','',li)
  li <- sub(' /[ dD]+onn.*?onal data','',li)
  li <- gsub('Datum|Zeit|Messwert','',li)
  li <- li[grepl('[[:alnum:]]',li)]
  
  i <- grep('^HBCH',li)
  h$id <- sub('HBCH[[:alnum:]]?-([[:digit:]]+)-.*','\\1',li[i])
  li <- sub(h$id,'',li,fixed=ifelse(nchar(h$id)>0,T,F))
  li <- sub('^HBCH[[:alnum:]]?--[[:digit:]]+-[[:digit:]]+$','',li)
  
  i <- grep('Abfluss|Wasserstand|Wassertemperatur',li)
  h$var <- sub('^.*?(Abfluss.*$|Wasserstand.*$|Wassertemperatur.*$)','\\1',li[i])
  li <- sub(h$var,'',li,fixed=ifelse(nchar(h$var)>0,T,F))
  
  i <- grep('^[[:alpha:][:blank:]]+,[[:alpha:][:blank:]]+-$',li)
  if(length(i) == 1L) {
    h$name <- sub('^([[:alpha:][:blank:]]+),[[:alpha:][:blank:]]+-$','\\1',li[i])
    h$site <- sub('^[[:alpha:][:blank:]]+,([[:alpha:][:blank:]]+)-$','\\1',li[i])
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
  
  h <- bafuHeader(h)
  return(h)
  
}

bafuHeaderXls <-
function(li=NULL) {
  
  h <- list(id=NA,name=NA,site=NA,var=NA,comment=NA,skip=NA)
  
  li <- gsub('Luecke|L\u00FCcke|L\\xfccke|NA|Lcke|L\u0081cke','',li)
  sk <- grep('^[[:blank:][:digit:][:cntrl:];,.:/-]+$',li)[1]-1
  li <- li[1:sk]
  li <- gsub('([*%";[:cntrl:]])','',li)
  li <- gsub('KennNr,Jahr,Maximum,Datum','',li,fixed=T)
  li <- li[grepl('[[:alnum:]]',li)]
  
  i <- grepl('^.*?Nr\\.[[:blank:][:digit:]]+',li)
  h$id <- sub('^.*?Nr[[:blank:][:punct:]]*([[:digit:]]+).*$','\\1',li[i])
  h$name <- sub('^.*?Nr[[:blank:][:punct:][:digit:]]+(.*?)-.*$','\\1',li[i])
  h$site <- sub('^.*?(-.*?)[[:blank:]]+in[[:blank:]]+.*$','\\1',li[i])
  h$var <- sub('^.*?[[:blank:]]+in[[:blank:]]+(.*?),+.*$','\\1',li[i])
  li <- sub(h$id,'',li,fixed=ifelse(nchar(h$id)>0,T,F))
  li <- sub(h$name,'',li,fixed=ifelse(nchar(h$name)>0,T,F))
  li <- sub(h$site,'',li,fixed=ifelse(nchar(h$site)>0,T,F))
  li <- sub(h$var,'',li,fixed=ifelse(nchar(h$var)>0,T,F))
  
  li <- gsub('^[[:blank:][:punct:][:cntrl:];,.:/-]+','',li)
  li <- gsub('[[:blank:][:punct:][:cntrl:];,.:/-]+$','',li)
  li <- li[grepl('[[:alnum:]]',li)]
  h$comment <- paste(li,collapse=' // ')
  h <- lapply(h,function(x){sub(' der Messstation Nr.   in','',x,fixed=T)})
  h <- lapply(h,function(x){sub('^[[:blank:][:cntrl:];,.:/-]+','',x)})
  h <- lapply(h,function(x){sub('[[:blank:][:cntrl:];,.:/-]+$','',x)})
  h$skip <- sk
  
  h <- bafuHeader(h)
  return(h)
  
}