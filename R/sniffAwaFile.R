sniffAwaFile <-
function(f=NULL) {

  h <- list(type=NA,id=NA,start=NA,end=NA,skip=0,sep=NA,var=NA,time='irregular',unit=NA,comment=NA)
  h['type'] <- sub('^.*?(\\..*$)','\\1',f)
  n <- capture.output(tmp <- scan(f,sep='\n',what=list(NULL),blank.lines.skip=F),type='message')
  n <- as.numeric(sub('^.*?([[:digit:]]+).*$','\\1',n))
  l1 <- scan(f,sep='\n',n=30,what='raw',blank.lines.skip=F,fileEncoding='CP1282',quiet=T)
  l2 <- scan(f,sep='\n',n=1,what='raw',blank.lines.skip=F,fileEncoding='CP1282',skip=n-1,quiet=T)
  n <- grep('^[[:blank:][:digit:]\t;,.:-]+$',l1)[1]
  h['skip'] <- n-1
  h['comment'] <- gsub(';','',paste(l1[1:(n-1)],collapse=' '))
  h['id'] <- sub('^.*?-([AGP]{1}[0123456789]+)-.*','\\1',h['comment'])
  if(grepl(';',l1[n])) {h['sep'] <- ';'}
  if(grepl(',',l1[n])) {h['sep'] <- ','}
  if(grepl('\t',l1[n])) {h['sep'] <- '\t'}
  if(any(grepl('(Wsfl|[wW]asserstand|m[ .]*?\u00FC[ .]*?[Mm])',l1))) {h['unit'] <- 'L'}
  if(any(grepl('([wW]assertemperatur|[tT]emperatur|\\xb0C|\u00B0C)',l1))) {h['unit'] <- 'T'}
  if(any(grepl('([aA]bfl|[aA]bfluss|m\\xb3/s|m\u00B3/s|m3/s|m.{1}/s)',l1))) {h['unit'] <- 'Q'}
  d <- sub(paste(h['sep'],'[[:digit:]','.,',h['sep'],']+$',sep=''),'',c(l1[n],l2))
  h[c('start','end')] <- gsub(h['sep'],' ',d)
  return(h)
  
}