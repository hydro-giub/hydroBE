\name{bafuFile}
\alias{bafuFile}
\alias{checkBafuFiles}
\alias{importBafuFiles}

\title{
Sniff in or import from Bafu files
}

\description{
This functions are mainly wrappers for \code{\link{bafuHeader}} and \code{\link{checkFiles}}. \code{checkBafuFiles} tries to summarize the content of one or several files from the Bafu (Bundesamt fuer Umwelt). This summary is then used by \code{importBafuFiles}.
}

\usage{
checkBafuFiles(dir=NULL, files=NULL, check.series=FALSE, 
               check.na=FALSE)
importBafuFiles(dir = NULL, files = NULL, 
                ignore.var = FALSE, quiet = TRUE)
}

\arguments{
\item{dir}{string, directory which contains files from Bafu}
\item{files}{character vector, file names}
\item{check.series}{logical, check series for start/end dates and temporal resolution?}
\item{check.na}{logical, check series for missing values? Leading and trailing \code{NA}s will be ignored. Applies only if \code{check.series=T}.}
\item{ignore.var}{logical, ignore any other attributes than the station id to merge series?}
\item{quiet}{logical, print summary?}
}

\details{
Either \code{dir} or \code{files} can be specified, but not both; \code{dir} takes precedence. 

The ordering of files in \code{dir} or \code{files} determines how \code{importBafuFiles} merges series: If station ids (and corresponding units for \code{ignore.var=F}) are consistent, then missing values in the first series are replaced by corresponding non-missing values in the second series.

\code{ignore.var=T} can be useful to force the merging of series (e.g. when \code{\link{bafuHeader}} returns "type=peak" and "type=unknown" for series from the same station).
}

\value{
In case of \code{checkBafuFiles} a matrix with columns "id" (station id), "name" (watercourse), "site" (site name), "var" (observed quantity), "res" (temporal resolution), "type" (peak versus mean values), "start" (date of first non-missing value), "end" (date of last non-missing value), "nas" (number of missing values, excluding leading and trailing missing values), "comment" (remaining annotations as found in the header), "file" (file name), "format" (file format), "hydropro" (file exported out of the Hydropro software?), "sep" (field separator), "skip" (number of header lines), and "nlines" (number of lines in the corresponding file).

In case of \code{importBafuFiles} a matrix with row names corresponding to the underlying time scale. In case of mean values, clock time refers to the starting time of the underlying period, e.g. for hourly values 01 corresponds to 01:00 - 01:59. The column names consist of "id", "type", and "var" as obtained from \code{checkBafuFiles}.
}

\section{Warning}{
\enumerate{
\item \code{check.na=T} requires to read the whole series and thus might be time consuming for large files.
\item Use \code{ignore.var=T} with caution, otherwise series with different variables (e.g. water temperature and streamflow or peak and mean values) might be merged.
\item Irregular series (Stuetzpunkte in Bafu parlance) are not supported by \code{importBafuFiles}.
\item See also the warnings for \code{\link{bafuHeader}} as well as \code{\link{bafuSeries}}.
}
}

\references{
\href{http://www.bafu.admin.ch/index.html?lang=en}{Bafu}
}

\author{
Simon S
}

\examples{
\dontrun{
d <- 'path/to/dir/'
m <- checkBafuFiles(dir=d)

d <- '/home/Downloads/'
df <- importBafuFiles(dir=d)

f <- c('/the/first/Bafu/file1.asc','/the/second/Bafu/file2.asc')
df <- importBafuFiles(files=f)
}
}