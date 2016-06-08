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

\details{
\code{awaHeader} only tries to prettify some output of \code{awaHeaderHydropro}.
}

\value{
List containing:
\item{id}{string, station id}
\item{name}{string, name of watercourse}
\item{site}{string, site name}
\item{var}{string, observed variable}
\item{comment}{string, remaining annotations}
\item{skip}{integer, number of header lines}
}

\section{Warning}{
The functions rely on a rather heuristic approach and thus may fail when the Awa comes up with new header formats. Particularly files out of the Hydropro software show a surprising wide range of naming conventions and non ascii characters.
}

\author{
Simon S
}

\seealso{
\code{\link{awaSeries}} which does the job for the lines following the header. \code{\link{checkAwaFiles}} and \code{\link{importAwaFiles}} which both rely on \code{awaHeader}.
}

\examples{
\dontrun{
co <- file('/path/to/file.asc',encoding='CP1282')
li <- readLines(con=co,n=30)
close(co)
h <- awaHeaderHydropro(li)
}
}
