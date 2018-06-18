# hydroBE
R-Package with miscellaneous functions of the [Group of Hydrology Bern](http://www.hydrologie.unibe.ch/index.html).

### How to start
To install the package, you can use the help of [{devtools}](https://CRAN.R-project.org/package=devtools):

```r
install.packages('devtools')
library('devtools')
install_github(repo='hydro-giub/hydroBE')
library(hydroBE)
package?hydroBE
```

or download the repo and install from source.

### Note
The package imports the [{trend}](https://CRAN.R-project.org/package=trend) package and its dependencies.
