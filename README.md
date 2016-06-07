# hydroBE
R-Package with miscellaneous functions for hydrologists workaday life.

To install the package, you can use the help of devtools:

```r
install.packages('devtools')
library('devtools')
install_github(repo='hydro-giub/hydroBE')
library(hydroBE)
?hydroBE
```

Or simply do it by hand:

```r
p <- '/path/to/your/Downloads/'
download.file('https://github.com/hydro-giub/hydroBE/archive/master.zip',destfile=paste(p,'master.zip',sep=''))
unzip(paste(p,'master.zip',sep=''),exdir=p)
if (!require(gdata)) {install.packages('gdata')}
if (!require(raster)) {install.packages('raster')}
install.packages(paste(p,'hydroBE-master/',sep=''),repo=NULL,type='source')
library(hydroBE)
?hydroBE
```
