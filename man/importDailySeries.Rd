\name{importDailySeries}
\alias{importDailySeriesBafu}
\alias{importDailySeriesBmlfuw}
\alias{importDailySeriesGrdc}
\alias{importDailySeriesLfub}
\alias{importDailySeriesLubw}
\alias{importDailySeriesMs}

\title{
Import daily series
}

\description{
The following data providers are supported at the moment: Bundesamt fuer Umwelt Schweiz
(Bafu); Bundesministerium fuer Land- und Forstwirtschaft, Umwelt und
Wasserwirtschaft Oesterreich (Bmlfuw); Global Runoff Data Centre (Grdc)
at Koblenz, Germany; Landesamt fuer Umwelt Bayern (Lfub); Landesanstalt
fuer Umwelt, Messungen und Naturschutz Baden-Wuerttemberg (Lubw);
Bundesamt fuer Meteorlogie und Klimatologie Schweiz (Ms).
}

\usage{
importDailySeriesBafu(file = NULL, id = NULL, series = T, quiet = T)
importDailySeriesBmlfuw(file = NULL, id = NULL, series = T, quiet = T)
importDailySeriesGrdc(file = NULL, id = NULL, series = T, quiet = T)
importDailySeriesLfub(file = NULL, id = NULL, series = T, quiet = T)
importDailySeriesLubw(file = NULL, id = NULL, series = T, quiet = T)
importDailySeriesMs(file = NULL, id = NULL, series = T, quiet = T, vars
= NULL, cnames=NULL) 
}

\arguments{
\item{file}{filename (can also be a regular expression)}
\item{id}{integer; id of the corresponding gauge station as specified by
  the provider}
\item{series}{logical; return the series? Otherwise, name, id, and
  filename are returned}
\item{quiet}{logical; print a short summary?}
\item{vars}{character vector; the variables column names inf
  \code{file}}
\item{cnames}{character vector of same length as \code{vars}; if
  supplied the returned matrix will have column names according
  \code{cnames}}
}

\details{
\code{id} gets verified against header data; daily regularity is not
guaranteed, but throws a warning if violated; leading and trailing NAs
get removed. If \code{file} is a regular expression, more than one
matches results in an error.
}

\value{
Matrix with the daily dates in the row names; streamflow data is labeled
'qd'. If \code{series=F} a list which contains some information about
the file.
}

\references{
\href{http://www.bafu.admin.ch/index.html?lang=en}{Bafu},
\href{http://ehyd.gv.at/}{Bmlfuw},
\href{http://www.bafg.de/GRDC/EN/Home/homepage_node.html}{Grdc},
\href{http://www.gkd.bayern.de/fluesse/abfluss/karten/index.php?thema=gkd&rubrik=fluesse&produkt=abfluss&gknr=0}{Lfub},
\href{https://www.lubw.baden-wuerttemberg.de/lubw}{Lubw},
\href{http://www.meteoswiss.admin.ch/home/services-and-publications/beratung-und-service/data-portal-for-teaching-and-research.html}{Ms}
}

\author{
Simon S
}

\note{
Daily series from the Bafu can also be (more convenient) imported via
\code{\link{importBafuFiles}}, but at the cost of an increased run time.
}

\examples{
# to do
}