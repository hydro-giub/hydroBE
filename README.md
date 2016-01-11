# hydroBE
R-Package with miscellaneous functions for hydrologists workaday life

To install the package, you can use the help of devtools

```r
install.packages('devtools')
library('devtools')
install_github(repo='hydro-giub/hydroBE')
library(hydroBE)
?hydroBE
```

or simply do it by hand with

```r
p <- '/path/to/your/Downloads/'
download.file('https://github.com/hydro-giub/hydroBE/archive/master.zip',destfile=paste(p,'master.zip',sep=''))
unzip(paste(p,'master.zip',sep=''),exdir=p)
install.packages(paste(p,'hydroBE-master/',sep=''),repo=NULL,type='source')
library(hydroBE)
?hydroBE
```
