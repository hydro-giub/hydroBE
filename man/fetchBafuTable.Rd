\name{fetchBafuTable}
\alias{fetchBafuTable}

\title{
  Grab daily streamflow series out of BAFU pdf tables
}

\description{
  The function returns daily streamflow series from pdf tables published
  by the BAFU (Bundesamt fuer Umwelt Schweiz) on the internet.
}

\usage{
fetchBafuTable(id, year)
}

\arguments{
  \item{id}{integer. The id of the gauging station of interest.}
  \item{year}{integer. The year for which to fetch the daily series.}
}

\value{
  Data frame with columns 'date' (daily time steps), 'discharge' (daily
  mean streamflow), and 'preliminary'. The latter indicates whether the
  values are quality controlled and homogenised.
}

\note{
  For a few gauging stations the pdf tables have a format different from
  the ones normally used. This most likely results in an error and holds
  in particular for the station at Aare Bruegg or some pdf tables
  containing data before the year 2004.
}

\author{
  Original code provided by Jan S.
}

\examples{
\dontrun{
## Muota at Ingenbohl
d <- fetchBafuTable(id='2084',year=2010)
head(d)}
}