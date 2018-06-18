\name{readDailySeries}
\alias{readDailySeries}

\title{
  Read daily time series from various hydrometeorological data providers
}

\description{
  The following data providers are supported:
  \tabular{rl}{
    \href{https://www.bafu.admin.ch/bafu/de/home.html}{BAFU} \tab (Switzerland) \cr
    \href{http://ehyd.gv.at/}{BMLFUW} \tab (Austria) \cr
    \href{http://www.bafg.de/GRDC/EN/Home/homepage_node.html}{GRDC} \tab (Germany) \cr
    \href{http://www.gkd.bayern.de/fluesse/abfluss/karten/index.php?thema=gkd}{LFUB} \tab (Germany) \cr
    \href{https://www.lubw.baden-wuerttemberg.de/lubw}{LUBW} \tab (Germany) \cr
    \href{http://sig.mapama.es/redes-seguimiento/visor.html?herramienta=Aforos}{MAPAMA} \tab (Spain) \cr
    \href{http://www.hydro.eaufrance.fr/}{MEDDE} \tab (France) \cr
    \href{http://www.meteoschweiz.admin.ch/home.html?tab=overview}{METSW} \tab (Switzerland)
  }
}

\usage{
readDailySeries(file = NULL, id = NULL, provider = NULL, series = TRUE,
                vars = NULL, cnames=NULL)
}

\arguments{
  \item{file}{string. The full file name (can also be a regular
    expression).}
  \item{id}{integer. Id of the corresponding gauge station as specified by
    the provider.}
  \item{provider}{string. The abbreviated name of the data provider as
    specified in the table above.}
  \item{series}{logical. Return the series? Otherwise, name, id, and
    file name are returned.}
  \item{vars}{character vector. The variable's column names in
    \code{file}. Currently only used for METSW.}
  \item{cnames}{character vector of same length as \code{vars}. If
    supplied the returned matrix will have column names according to
    \code{cnames}. Currently only used for METSW.}
}

\details{
  \itemize{
    \item The \code{id} gets verified against the header data.
    \item Daily regularity is not guaranteed, but throws a warning if
    violated.
    \item Leading and trailing NAs get removed.
    \item If \code{file} is a regular expression, more than one match
    results in an error. 
  }
}

\value{
  Matrix with the daily dates in the row names; streamflow data is labeled
  'qd'. If \code{series=FALSE} a list that contains some information about
  the file.
}

\section{Warning}{
  This function has been successfully used over the past few years,
  however, it will fail as soon as the data providers change their file
  structures or header formats.
}

\author{
Simon S
}

\examples{
\dontrun{
f <- '/path/to/file.txt'
d <- readDailySeries(file=f,id=2089,provider='BAFU')}
}
