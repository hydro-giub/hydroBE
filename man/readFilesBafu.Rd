\name{readFilesBafu}
\alias{readFilesBafu}

\title{
Read files from the BAFU
}

\description{
Summarise or read the content of files from the BAFU (Bundesamt fuer
Umwelt Schweiz).
}

\usage{
readFilesBafu(dir = NULL, files, time.res, series = FALSE, merge)
}

\arguments{
\item{dir}{string. the directory which contains the files from AWA.}
\item{files}{character vector. The file names.}
\item{time.res}{string. The measurement frequency, must be either
  'year', 'month', 'day', 'hour', 'min', or 'irregular', see 'Details'.}
\item{series}{logical. Return the series instead of a summary? In case
  of several files, this can take some time.}
\item{merge}{character vector. If not missing, the series get merged
  according to the attributes defined in \code{merge}, see 'Details'.}
}

\details{
Either \code{dir} or \code{files} can be specified, but not both;
\code{dir} takes precedence.

\code{time.res} determines how the function deals with the time
information. The approach is to first constrain the time stamps to the
format 'YYYY-MM-DD HH:MM' and second to truncate this string to the
desired resolution. E.g. for \code{time.res = 'month'} we end up with
'YYYY-MM' and for \code{time.res = 'hour'} we get 'YYYY-MM-DD HH'. In
case of \code{time.res = 'min'} and \code{time.res = 'irregular'} the
last digit is ignored, so the highest frequency amounts to 10 minute
intervals (i.e. 'YYYY-MM-DD HH:M').

\code{time.res} is in particular important if \code{series = TRUE} since
it forces the observations to a common time interval. If more than one
observation is present in a particular time interval, only the last
observation is used.

The ordering of files in \code{dir} or \code{files} determines how
series get merged: If the attributes specified in \code{merge} are
consistent, then missing values in the first series are replaced by
corresponding non-missing values in the second series and so
on. Possible attributes are restricted to column names of the data frame
returned if \code{series = FALSE}. Series for which an attribute is
missing are exluded from the mergin procedure.
}

\value{
If \code{series = FALSE} a data frame with columns 'file' (file name)
'id' (station id), 'name' (watercourse), 'site' (site name), 'unit'
(measurement unit), 'type' (peak versus mean values), 'start' (date of
the first non-missing value), 'end' (date of the last non-missing
value), 'comment' (remaining annotations as found in the header),
'hydropro' (file exported out of the Hydropro software?), 'sep' (field
separator), and 'skip' (number of header lines).

If \code{series = FALSE} a matrix with row names corresponding to the
argument \code{time.res}.  In case of mean values, clock time refers
to the starting time of the underlying period, e.g. for hourly values 01
corresponds to 01:00 - 01:59. The column names contain the id (if
present) of the station as last number.
}

\section{Warning}{
\itemize{
  \item The function relies on a rather heuristic approach and thus may
  fail when the BAFU comes up with new file formats.
  \item Warning messages like 'NAs introduced by coercion' mean that
  missing values are present in at least one of the series.
}
}

\references{
\href{https://www.bafu.admin.ch/bafu/en/home.html?_organization=811&_topic=29}{BAFU}
}

\author{
Simon S
}

\examples{
\dontrun{
d <- '/path/to/dir/'
readFilesBafu(dir=d)
m <- readFilesBafu(dir=d,series=TRUE)

f <- c('/the/first/Bafu/file1.asc','/the/second/Bafu/file2.asc')
m <- readFilesBafu(files=f,series=T)}
}
