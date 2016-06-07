\name{importBafuFiles}
\alias{importBafuFiles}

\title{
Import hydrological time series from Bafu
}

\description{
Imports hydrological time series from files provided by the Bundesamt fuer Umwelt (Bafu). Tries to check the variables consistency and to merge files if feasible.
}

\usage{
importBafuFiles(dir = NULL, files = NULL, 
                ignore.var = FALSE, quiet = TRUE)
}

\arguments{
\item{dir}{string, import all files in directory \code{dir}}
\item{files}{character vector, import all files specified in \code{files}}
\item{ignore.var}{logical, merge series only according to the station id?}
\item{quiet}{logical, print summary?}
}

\details{
Either \code{dir} or \code{files} can be specified, but not both; \code{dir} takes precedence. The ordering of files in \code{dir} or \code{files} determines how merging of the series is applied: If station id and corresponding variables (e.g. mean streamflow) are consistent, then missing values in the first series are replaced by corresponding values in the second series.
}

\value{
Matrix with row names corresponding to the underlying time scale. In case of mean values clock time refers to the starting time of the underlying period, e.g. for hourly values 01 corresponds to 01:00 - 01:59.
}

\section{Warning}{
\enumerate{
\item Irregular series (Stuetzpunkte in Bafu parlance) are not supported.
\item Use \code{ignore.var=T} with caution, otherwise series with different variables (e.g. water temperature and streamflow or peak and mean values) might be merged. 
\item The function tries hard to identify the variables (i.e. peak versus mean values and water levels versus discharge) as well as the underlying time scale. However, \code{importBafuFiles} assumes a certain file structure (e.g. header and date format, text delimiter) and thus might fail to successfully check the files consistency when these assumptions do not hold. It is highly recommended to make some validation against the original files.
}
}

\references{
\href{http://www.bafu.admin.ch/index.html?lang=en}{Bafu}
}

\author{
Simon
}

\note{
\code{ignore.var=T} can be useful to update series with values from files out of the Hydropro software.
}

\examples{
d <- '/home/Downloads/'
\dontrun{df <- importBafuFiles(dir=d)}   

f <- c('/the/first/Bafu/file1.asc','/the/second/Bafu/file2.asc')
\dontrun{df <- importBafuFiles(files=f)} 
}
