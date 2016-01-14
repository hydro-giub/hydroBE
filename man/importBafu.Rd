\name{importBafu}
\alias{importBafu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import time series from Bundesamt fuer Umwelt (Bafu)
}
\description{
to do
}
\usage{
importBafu(dir = NULL, files = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{string. Import all files in directory \code{dir}.}
  \item{files}{character vector. Import all files specified in \code{files}.
}
}
\details{
Either \code{dir} or \code{files} can be specified, but not both. \code{dir} takes precedence.
}
\value{
data frame with column 'time' followed by corresponding observations.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Simon
}
\note{
The function tries hard to identify the variables (i.e. peaks versus mean values) and underlying time units in the provided files. However, \code{importBafu} 
assumes a certain file structure (e.g. header format and text delimiter) and thus might fail to successfully check the files consistency. 
For example, the Bafu sometimes has to export the most recent observations directly out of the HydroPro software. In this case, the user is responsible
to check the validity of the imported data.

For convenience column 'time' has class \code{\link{POSIXct}}. In case of mean values, time refers here to the starting time of the underlying period, e.g. in case
of hourly mean values 01:00 corresponds to 01:00 - 02:00.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
a <- 1+1
}
