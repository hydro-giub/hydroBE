\name{importAwa}
\alias{importAwa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import time series from Awa
}
\description{
Imports time series from files provided by the Amt fuer Wasser und Abfall - Kanton Bern (Awa). Tries to check the variables consistency and to merge files if feasible.
Also the values get aggregated in time (i.e. averaged) in order to return a strictly regular series.
}
\usage{
importAwa(dir = NULL, files = NULL, agg = 'day', quiet = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{string. Import all files in directory \code{dir}.}
  \item{files}{character vector. Import all files specified in \code{files}.}
  \item{agg}{string. Favoured time aggregation. One of '10 min', 'hour', 'day', 'month', or 'year'. Defaults to 'day'.}
  \item{quiet}{logical. Print summary?}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
data frame with column 'time' followed by corresponding observations. Depending on the underlying time scale, column 'time' is either an
integer vector (in case of yearly values), character vector (in case of monthly values), or of class \code{\link{POSIXct}} (in case of at least daily
resolution). When of class \code{\link{POSIXct}}, clock time refers to the starting time of the underlying period, e.g. for 
hourly values 01:00 corresponds to 01:00 - 02:00.
}
\references{
\href{http://www.bve.be.ch/bve/de/index/direktion/organisation/awa.html#originRequestUrl=www.be.ch/awa}{Awa}
}
\author{
Simon
}
\note{
%%  ~~further notes~~
}
\section{Warning}{
The function does not test if \code{agg} is agreeable with the amount of available observations. Thus yearly values might be based on only a few observations.
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
d <- '/home/Downloads/'
\dontrun{df <- importAwa(dir=d)}   

f <- c('/the/first/Awa/file1.asc','/the/second/Awa/file2.asc')
\dontrun{df <- importAwa(files=f)} 
}