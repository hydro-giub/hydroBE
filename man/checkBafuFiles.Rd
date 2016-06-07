\name{checkBafuFiles}
\alias{checkBafuFiles}

\title{
Sniff in files provided by Bafu
}

\description{
This function is a wrapper for \code{\link{bafuHeader}} and \code{\link{bafuSeries}}. It tries to summarize the content of one or several files from Bafu (Bundesamt fuer Umwelt). This summary might be useful to subsequently import and/or merge the files.
}

\usage{
checkBafuFiles(dir=NULL, files=NULL, check.series=FALSE, 
               check.na=FALSE)
}

\arguments{
\item{dir}{string, directory which contains files from Bafu}
\item{files}{character vector, file names}
\item{check.series}{logical, check series for start/end dates, temporal resolution, or missing values? Overrides \code{check.na}.}
\item{check.na}{logical, check series for missing values? Passed to \code{\link{bafuSeries}}; leading and trailing \code{NA}s will be ignored.}
}

\details{
Either \code{dir} or \code{files} can be specified, but not both; \code{dir} takes precedence. \code{check.na=T} requires also that \code{check.series=T}, otherwise only the file headers are checked.
}

\value{
Matrix with columns "id" (station id), "name" (watercourse), "site" (site name), "var" (observed quantity), "res" (temporal resolution), "type" (peak versus mean values), "start" (date of first non-missing value), "end" (date of last non-missing value), "nas" (number of missing values, excluding leading and trailing missing values), "comment" (remaining annotations as found in the header), "file" (file name), "format" (file format), "hydropro" (file exported out of the Hydropro software?), "sep" (field separator), "skip" (number of header lines), and "nlines" (number of lines in the corresponding file).
}

\section{Warning}{
\enumerate{
\item \code{check.na=T} requires to read the whole series and thus might be time consuming for larger files.
\item The approach for guessing the temporal resolution of the series might fail when the series is irregular (i.e. when missing values are not declared, but instead the corresponding rows are omitted).
\item Since character encodings might not be consistent and the Bafu enjoys to use non ascii characters, it is not guaranteed that the files are properly decoded.
}
}

\author{
Simon
}

\seealso{
\code{\link{importBafuFiles}}
}

\examples{
\dontrun{
d <- 'path/to/dir/'
m <- checkBafuFiles(dir=d)
}
}