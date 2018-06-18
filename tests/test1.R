## test cleanSeries*()
library(hydroBE)
rm(list=ls())

check <- function(f,x,sep,value,date) {

    n <- length(x)
    r <- matrix('',ncol=2,nrow=n)
    for(i in 1:n) {
        tryCatch(
        {
            rt <- do.call(f,args=list(x=x[i],sep=sep))
            r[i,1] <- rt
            r[i,2] <- names(rt)
        },
        error=function(cond){},
        warning=function(cond){})
    }
    p <- T
    if(any(r[,1]!=value)) p <- F
    if(any(r[,2]!=date)) p <- F
    if(p) {
        return(p)
    } else {
        return(r)
    }

}

## AWA and BAFU hydropro date and time
d <- c('31.12.1999,24:00,1',
       '01.01.2000,00:00,1',
       '01.01.2000,00:00,1',
       ' 01.01.2000,00:00,1',
       '01.01.2000 ,00:00,1',
       '01.01.2000, 00:00,1',
       '01.01.2000,00:00 ,1',
       '01.01.2000,00:00, 1',
       '01.01.2000,00:00,1 ',
       '01.01.2000,00:00 , 1')
r <- check(f=hydroBE:::.cleanSeriesAwaHydropro,x=d,sep=',',
           value='1',date='2000-01-01 00:00')
if(!isTRUE(r)) {print(r);stop('.cleanSeriesAwaHydropro')}
r <- check(f=hydroBE:::.cleanSeriesBafuHydropro,x=d,sep=',',
           value='1',date='2000-01-01 00:00')
if(!isTRUE(r)) {print(r);stop('.cleanSeriesBafuHydropro')}

## AWA test removing of unnecessary columns
d <- c('01.01.2000,00:00,1',
       '01.01.2000,00:00,1,1,1,',
       '01.01.2000,00:00, 1,  2; 5',
       '01.01.2000,00:00,1   ,.3',
       '01.01.2000,00:00,1;3',
       '01.01.2000,00:00,1,',
       '01.01.2000,00:00,1,,',
       '01.01.2000,00:00,1,,,',
       '01.01.2000,00:00,1,Luecke',
       '01.01.2000,00:00,1,.2,3,,')
r <- check(f=hydroBE:::.cleanSeriesAwaHydropro,x=d,sep=',',
           value='1',date='2000-01-01 00:00')
if(!isTRUE(r)) {print(r);stop('.cleanSeriesAwaHydropro')}

## BAFU regular date and time, mean values
d <- c('0050;2000.01-2000.01;1',
       '0050;2000.01.01-2000.01.02;1',
       '0050;2000.01.01 00:00-2000.01.01 01:00;1',
       '0050;2000.01.01 00:00:00-2000.01.01 01:00:00;1',
       '0050;2000.01-2000.01;  1',
       '0050;2000.01-2000.01;    1',
       '0050;2000.01-2000.01;   1    ',
       '0050; 2000.01.01-2000.01.02;1',
       '0050;  2000.01.01-2000.01.02;1',
       '0050;2000.01.01 - 2000.01.02;1',
       '0050;2000.01.01  - 2000.01.02;1',
       '0050;2000.01.01-2000.01.02  ;1',
       ';2000.01.01 00:00-2000.01.01 01:00;1',
       '2000.01.01 00:00-2000.01.01 01:00;1')
r <- check(f=hydroBE:::.cleanSeriesBafuRegular,x=d,sep=';',
           value='1',date='2000-01-01 00:00')
if(!isTRUE(r)) {print(r);stop('.cleanSeriesBafuRegular')}

## BAFU regular date and time, peak values
d <- c('0050;2000.01;1',
       '0050;2000.01.01;1',
       '0050;2000.01.01 00:00;1',
       '0050;2000.01.01 00:00:00;1',
       '0050;2000.01;  1',
       '0050;2000.01;    1',
       '0050;2000.01;   1    ',
       '0050; 2000.01.01;1',
       '0050;  2000.01.01;1',
       '0050;2000.01.01 ;1',
       '0050;2000.01.01  ;1',
       '0050;2000.01.01  ;1',
       ';2000.01.01 00:00;1',
       '2000.01.01 00:00;1')
r <- check(f=hydroBE:::.cleanSeriesBafuRegular,x=d,sep=';',
           value='1',date='2000-01-01 00:00')
if(!isTRUE(r)) {print(r);stop('.cleanSeriesBafuRegular')}

## AWA and BAFU hydropro missing values
d <- c('01.01.2000,00:00,NA',
       '01.01.2000,00:00,Luecke',
       '01.01.2000,00:00,L\u00FCcke',
       '01.01.2000,00:00,L\\xfccke',
       '01.01.2000,00:00,Lcke',
       '01.01.2000,00:00,L\u0081cke',
       '01.01.2000,00:00,',
       '01.01.2000,00:00,  ',
       '01.01.2000,00:00,-',
       '01.01.2000,00:00,.',
       '01.01.2000,00:00,1.L',
       '01.01.2000,00:00,L1',
       '01.01.2000,00:00,bla1',
       '01.01.2000,00:00,1-1')
r <- check(f=hydroBE:::.cleanSeriesAwaHydropro,x=d,sep=',',
           value='',date='')
if(!isTRUE(r)) {print(r);stop('.cleanSeriesAwaHydropro')}
r <- check(f=hydroBE:::.cleanSeriesBafuHydropro,x=d,sep=',',
           value='',date='')
if(!isTRUE(r)) {print(r);stop('.cleanSeriesBafuHydropro')}
