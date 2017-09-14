\name{importDailySeries}
\alias{importDailySeriesBafu}
\alias{importDailySeriesBmlfuw}
\alias{importDailySeriesGrdc}
\alias{importDailySeriesLfub}
\alias{importDailySeriesLubw}
\alias{importDailySeriesMapama}
\alias{importDailySeriesMs}

\title{
  Import daily series from various providers
}

\description{
  The following data providers are supported:
  \tabular{rl}{
    \href{https://www.bafu.admin.ch/bafu/de/home.html}{BAFU} \tab Switzerland \cr
    \href{http://ehyd.gv.at/}{BMLFUW} \tab Austria \cr
    \href{http://www.bafg.de/GRDC/EN/Home/homepage_node.html}{GRDC} \tab Germany \cr
    \href{http://www.gkd.bayern.de/fluesse/abfluss/karten/index.php?thema=gkd}{LFUB} \tab Germany \cr
    \href{https://www.lubw.baden-wuerttemberg.de/lubw}{LUBW} \tab Germany \cr
    \href{http://sig.mapama.es/redes-seguimiento/visor.html?herramienta=Aforos}{MAPAMA} \tab Spain \cr
    \href{http://www.meteoschweiz.admin.ch/home.html?tab=overview}{MS} \tab Switzerland
  }
}

\usage{
importDailySeriesBafu(file = NULL, id = NULL, series = TRUE, quiet = TRUE)
importDailySeriesBmlfuw(file = NULL, id = NULL, series = TRUE, quiet = TRUE)
importDailySeriesGrdc(file = NULL, id = NULL, series = TRUE, quiet = TRUE)
importDailySeriesLfub(file = NULL, id = NULL, series = TRUE, quiet = TRUE)
importDailySeriesLubw(file = NULL, id = NULL, series = TRUE, quiet = TRUE)
importDailySeriesMapama(file = NULL, id = NULL, series = TRUE, quiet = TRUE)
importDailySeriesMs(file = NULL, id = NULL, series = TRUE, quiet = TRUE,
                    vars = NULL, cnames=NULL)
}

\arguments{
  \item{file}{string. The full file name (can also be a regular
    expression).}
  \item{id}{integer. Id of the corresponding gauge station as specified by
    the provider.}
  \item{series}{logical. Return the series? Otherwise, name, id, and
    file name are returned.}
  \item{quiet}{logical. Print a short summary?}
  \item{vars}{character vector. The variable's column names in
    \code{file}.}
  \item{cnames}{character vector of same length as \code{vars}. If
    supplied the returned matrix will have column names according to
    \code{cnames}.}
}

\details{
  The \code{id} gets verified against header data; daily regularity is not
  guaranteed, but throws a warning if violated; leading and trailing NAs
  get removed. If \code{file} is a regular expression, more than one
  match results in an error.
}

\value{
  Matrix with the daily dates in the row names; streamflow data is labeled
  'qd'. If \code{series=FALSE} a list which contains some information about
  the file.
}

\author{
Simon S
}
