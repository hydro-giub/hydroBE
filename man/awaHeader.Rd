\name{awaHeader}
\alias{awaHeader}
\alias{awaHeaderHydropro}

\title{
Sniff in the header of a Awa file
}

\description{
Helper functions to extract header information out of files from Awa (Amt fuer Wasser und Abfall Kanton Bern).
}

\usage{
awaHeader(h = NULL)
awaHeaderHydropro(li = NULL)
}

\arguments{
\item{h}{list as returned by \code{awaHeaderHydropro}}
\item{li}{character vector containing the first lines of a Awa file}
}

\value{
List containing:
\item{id}{string, gauge id}
\item{name}{string, name of watercourse}
\item{site}{string, site name}
\item{var}{string, observed variable}
\item{comment}{string, remaining annotations}
\item{skip}{integer, number of header lines}
}

\author{
Simon
}

\note{
The function relies on a rather heuristic approach to extract useful information and thus might fail when the Awa comes up with new formats. Particularly files out of the Hydropro software (used for the most recent and unofficial observations) show a surprising wide range of naming conventions and non ascii characters.

\code{awaHeader} only tries to prettify the output of \code{awaHeaderHydropro}.
}

\seealso{
\code{\link{awaSeries}} which does the job for the lines following the header. \code{\link{checkAwaFiles}} and \code{\link{importAwaFiles}} which both rely on \code{awaHeader}.
}

\examples{
\dontrun{
co <- file('/path/to/file.asc',encoding='ISO-8859-1')
li <- readLines(con=co,n=30)
close(co)
h <- awaHeaderHydropro(li)
}
}