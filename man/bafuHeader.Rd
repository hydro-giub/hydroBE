\name{bafuHeader}
\alias{bafuHeader}
\alias{bafuHeaderHydropro}
\alias{bafuHeaderTxt}
\alias{bafuHeaderXls}

\title{
Sniff in the header of a Bafu file
}

\description{
Helper functions to extract header information out of files from Bafu (Bundesamt fuer Umwelt) 
}

\usage{
bafuHeader(h = NULL)
bafuHeaderHydropro(li = NULL)
bafuHeaderTxt(li = NULL)
bafuHeaderXls(li = NULL)
}

\arguments{
\item{h}{list as returned by \code{bafuHeaderHydropro}, \code{bafuHeaderTxt}, or \code{bafuHeaderXls}}
\item{li}{character vector containing the first lines of a Bafu file}
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
The function relies on a rather heuristic approach to extract useful information and thus might fail when the Bafu comes up with new formats. Particularly files out of the Hydropro software (used for the most recent and unofficial observations) show a surprising wide range of naming conventions and non ascii characters.

\code{bafuHeader} only tries to prettify the output of \code{bafuHeaderHydropro}, \code{bafuHeaderTxt}, and \code{bafuHeaderXls}.
}

\seealso{
\code{\link{bafuSeries}} which does the job for the lines following the header. \code{\link{checkBafuFiles}} and \code{\link{importBafuFiles}} which both rely on \code{bafuHeader}.
}

\examples{
\dontrun{
co <- file('/path/to/file.asc',encoding='ISO-8859-1')
li <- readLines(con=co,n=30)
close(co)
if (any(grepl('[hH]ydro|[hH][bB][cC][hH]',li))) {
h <- bafuHeaderTxt(li)
} else {h <- bafuHeaderHydropro(li)}
}
}