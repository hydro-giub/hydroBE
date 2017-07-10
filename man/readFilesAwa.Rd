\name{readFilesAwa}
\alias{readFilesAwa}

\title{
Read files from the AWA
}

\description{
Summarise or read the content of files from the AWA (Amt fuer Wasser und
Abfall Kanton Bern).
}

\usage{
readFilesAwa(dir = NULL, files, time.res, series = FALSE, 
              merge = FALSE)
}

\arguments{
\item{dir}{string. the directory which contains the files from AWA.}
\item{files}{character vector. The file names.}
\item{time.res}{string. The measurement frequency, must be either
  'year', 'month', 'day', 'hour', 'min', or 'irregular', see 'Details'.}
\item{series}{logical. Return the series instead of a summary? In case
  of several files, this can take some time.}
\item{merge}{logical. Merge the series when observations of a gauging
  station are distributed over more than one file?}
}

\details{
Either \code{dir} or \code{files} can be specified, but not both;
\code{dir} takes precedence.

\code{time.res} determines how the function deals with the time
information. The approach is to first constrain the time stamps to the
format 'YYYY-MM-DD HH:MM' and second to truncate this string to the
desired resolution. E.g. for \code{time.res='month'} we end up with
'YYYY-MM' and for \code{time.res='hour'} we get 'YYYY-MM-DD HH'. In case
of \code{time.res='min'} and \code{time.res='irregular'} the last digit
is ignored, so the highest frequency amounts to 10 minute intervals
(i.e. 'YYYY-MM-DD HH:M').

\code{time.res} is in particular important if \code{series=TRUE} since
it forces the observations to a common time interval. If more than one
observation is present in a particular time interval, only the last
observation is used.

The ordering of files in \code{dir} or \code{files} determines how
series get merged: If station id, unit, and type are consistent, then
missing values in the first series are replaced by corresponding
non-missing values in the second series and so on.
}

\value{
If \code{series=FALSE} a data frame with columns 'file' (file name) 'id'
(station id), 'name' (watercourse), 'site' (site name), 'unit'
(measurement unit), 'type' (peak versus mean values), 'start' (date of
the first non-missing value), 'end' (date of the last non-missing
value), 'comment' (remaining annotations as found in the header),
'hydropro' (file exported out of the Hydropro software?), 'sep' (field
separator), and 'skip' (number of header lines).

If \code{series=FALSE} a matrix with row names corresponding to the
underlying time scale. In case of mean values, clock time refers
to the starting time of the underlying period, e.g. for hourly values 01
corresponds to 01:00 - 01:59. The column names contain the id (if
present) of the station as last number.
}

\section{Warning}{
The functions rely on a rather heuristic approach and thus may
fail when the AWA comes up with new file formats. Particularly files out
of the Hydropro software show a surprising wide range of naming
conventions and non ascii characters.

}

\references{
\href{http://www.bve.be.ch/bve/de/index/direktion/organisation/awa.html#originRequestUrl=www.be.ch/awa}{AWA}
}

\author{
Simon S
}

\examples{
\dontrun{
d <- '/path/to/dir/'
readFilesAwa(dir=d)
m <- readFilesAwa(dir=d,series=TRUE)

f <- c('/the/first/Awa/file1.asc','/the/second/Awa/file2.asc')
m <- readFilesAwa(files=f,series=T)}
}
