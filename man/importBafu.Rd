\name{importBafu}
\alias{importBafu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import time series from Bafu
}
\description{
Imports time series from files provided by the Bundesamt fuer Umwelt (Bafu). Tries to check the variables consistency and to merge files if feasible.
}
\usage{
importBafu(dir = NULL, files = NULL, quiet = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{string. Import all files in directory \code{dir}.}
  \item{files}{character vector. Import all files specified in \code{files}.}
  \item{quiet}{logical. Print summary?}
}
\details{
Either \code{dir} or \code{files} can be specified, but not both. \code{dir} takes precedence. The ordering of files in \code{dir} or \code{files}
determines how merging of series is applied: If station id, variable (i.e. peak vs. mean values), and unit are consistent, then missing values in the
first series are replaced by corresponding values in the second series.
}
\value{
data frame with column 'time' followed by corresponding observations. Depending on the underlying time scale, column 'time' is either an
integer vector (in case of yearly values), character vector (in case of monthly values), or of class \code{\link{POSIXct}} (in case of at least daily
resolution). When of class \code{\link{POSIXct}}, clock time refers to the starting time of the underlying period, e.g. for 
hourly values 01:00 corresponds to 01:00 - 02:00.
}
\references{
\href{http://www.bafu.admin.ch/index.html?lang=en}{Bafu}
}
\author{
Simon
}
\note{
The function tries hard to identify the variables (i.e. peaks versus mean values), underlying time units, and variables (e.g. levels or discharge). 
However, \code{importBafu} assumes a certain file structure (e.g. header and date format, text delimiter) and thus might fail to successfully check the files consistency
when these assumptions does not hold. It is highly recommended to make some validation against the original files.

The Bafu sometimes has to export the most recent observations directly out of the HydroPro software. As it is not clear if these observations are peaks or mean values, no
merging is applied.
}
\section{Warning}{
Irregular series (Stuetzpunkte in Bafu terminology) are not supported.
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
d <- '/home/Downloads/'
\dontrun{df <- importBafu(dir=d)}   

f <- c('/the/first/Bafu/file1.asc','/the/second/Bafu/file2.asc')
\dontrun{df <- importBafu(files=f)} 
}
