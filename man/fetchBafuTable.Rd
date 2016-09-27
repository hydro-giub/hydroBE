\name{fetchBafuTable}
\alias{fetchBafuTable}

\title{
Grab daily stream flow series out of Bafu pdf tables
}

\description{
Bafu: Bundesamt fuer Umwelt, Schweiz
}

\usage{
fetchBafuTable(id = NULL, year = NULL)
}

\arguments{
\item{id}{integer; gauge id}
\item{year}{integer; year for which to fetch the daily series}
}

\value{
data frame with columns \code{date} and \code{qd}
}

\note{
\code{year} should be greater than 2003.
}

\author{
Original code by Jan S, adapted by Simon S.
}


\examples{
# to do
}