\name{awaFile}
\alias{awaFile}
\alias{checkAwaFiles}
\alias{importAwaFiles}

\title{
Sniff in or import from Awa files
}

\description{
This functions are mainly wrappers for \code{\link{awaHeader}} and \code{\link{checkFiles}}. \code{checkAwaFiles} tries to summarize the content of one or several files from the Awa (Amt fuer Wasser und Abfall Kanton Bern)). This summary is then used by \code{importAwaFiles}.
}

\usage{
checkAwaFiles(dir=NULL, files=NULL, check.series=FALSE, 
              check.na=FALSE)
importAwaFiles(dir = NULL, files = NULL, ignore.var = FALSE, 
               simplify.time = FALSE, quiet = TRUE)
}

\arguments{
\item{dir}{string, directory which contains files from Awa}
\item{files}{character vector, file names}
\item{check.series}{logical, check series for start/end dates and temporal resolution?}
\item{check.na}{logical, check series for missing values? Leading and trailing \code{NA}s will be ignored. Applies only if \code{check.series=T}.}
\item{ignore.var}{logical, ignore any other attributes than the station id to merge series?}
\item{simplify.time}{logical, use YYYY.MM.DD HH:M to synchronise series?}
\item{quiet}{logical, print summary?}
}

\details{
Either \code{dir} or \code{files} can be specified, but not both; \code{dir} takes precedence. 

The ordering of files in \code{dir} or \code{files} determines how \code{importAwaFiles} merges series: If station ids (and corresponding units for \code{ignore.var=F}) are consistent, then missing values in the first series are replaced by corresponding non-missing values in the second series.

\code{ignore.var=T} can be useful to force the merging of series (e.g. when \code{\link{awaHeader}} returns "type=peak" and "type=unknown" for series from the same station).

\code{simplify.time=T} should be only used to truncate irregular series to regular 10 min series. When several observations are available in a particular 10 min interval, the last observation will be retained.
}

\value{
In case of \code{checkAwaFiles} a matrix with columns "id" (station id), "name" (watercourse), "site" (site name), "var" (observed quantity), "res" (temporal resolution), "type" (peak versus mean values), "start" (date of first non-missing value), "end" (date of last non-missing value), "nas" (number of missing values, excluding leading and trailing missing values), "comment" (remaining annotations as found in the header), "file" (file name), "format" (file format), "hydropro" (file exported out of the Hydropro software?), "sep" (field separator), "skip" (number of header lines), and "nlines" (number of text lines).

In case of \code{importAwaFiles} a matrix with row names corresponding to the underlying time scale. In case of mean values, clock time refers to the starting time of the underlying period, e.g. for hourly values 01 corresponds to 01:00 - 01:59. The column names consist of "id", "type", and "var" as obtained from \code{checkAwaFiles}.
}

\section{Warning}{
\enumerate{
\item \code{check.na=T} requires to read the whole series and thus might be time consuming for large files.
\item Use \code{ignore.var=T} with caution, otherwise series with different variables (e.g. water temperature and streamflow or peak and mean values) might be merged. 
\item See also the warnings for \code{\link{awaHeader}} as well as \code{\link{awaSeries}}.
}
}

\references{
\href{http://www.bve.be.ch/bve/de/index/direktion/organisation/awa.html#originRequestUrl=www.be.ch/awa}{Awa}
}

\author{
Simon S
}

\examples{
\dontrun{
d <- 'path/to/dir/'
m <- checkAwaFiles(dir=d)

d <- '/home/Downloads/'
df <- importAwaFiles(dir=d)

f <- c('/the/first/Awa/file1.asc','/the/second/Awa/file2.asc')
df <- importAwaFiles(files=f)
}
}
