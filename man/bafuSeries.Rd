\name{bafuSeries}
\alias{bafuSeries}
\alias{bafuSeriesHydropro}
\alias{bafuSeriesTxt}
\alias{bafuSeriesXls}

\title{
Sniff in Bafu time series
}

\description{
Helper functions to summarize hydrological time series provided by Bafu (Bundesamt fuer Umwelt). It is intended to provide a summary useful for the import of Bafu files, not a summary in the sense of a statistical description. 
}

\usage{
bafuSeriesHydropro(file = NULL, nlines = 0, skip = 3, 
                   check.na = FALSE, series.only = FALSE)
bafuSeriesTxt(file = NULL, nlines = 0, skip = 3, 
              check.na = FALSE, series.only = FALSE)
bafuSeriesXls(file = NULL, nlines = 0, skip = 2, 
              check.na = FALSE, series.only = FALSE)
}

\arguments{
\item{file}{string, file name}
\item{nlines}{integer, number of lines in \code{file}}
\item{skip}{integer, number of header lines}
\item{check.na}{logical, check series for missing values?}
\item{series.only}{logical, return series in a nx1 matrix instead of a summary? Timestamps will be attached as row names.}
}

\details{
In case of mean values, returned date and time information indicates the start of the corresponding period. E.g. for hourly mean values, "2015.09.01 01:00" depicts the interval 01:00-02:00 and for 10 min mean values "2015.09.01 01:00" depicts the interval 01:00-01:09.
}

\value{
\code{bafuSeries} returns a list with entries
\item{res}{string, temporal resolution (either 10 min, hourly, daily, monthly, yearly, irregular)}
\item{start}{string, date of the first non-missing observation}
\item{end}{string, date of the last non-missing observation}
\item{nas}{integer, number of missing values (leading and trailing \code{NA}s will be ignored)}
\item{type}{string, either "peak" or "mean" value}
\item{sep}{character, field separator in \code{file}}

If \code{series.only=T} \code{bafuSeriesHydropro}, \code{bafuSeriesTxt}, and \code{bafuSeriesXls} return a nx1 matrix.
}

\section{Warning}{
\enumerate{
\item \code{check.na=T} or \code{series.only=T} requires to read the whole series and thus might be time consuming for larger files.
\item The approach for guessing the temporal resolution of the series might fail when the series is irregular (i.e. when missing values are not declared, but instead the corresponding rows are omitted).
}
}

\author{
Simon S
}

\seealso{
\code{\link{bafuHeader}} which does the job for the header lines. \code{\link{checkBafuFiles}} and \code{\link{importBafuFiles}} which both rely on \code{bafuSeries}.
}

\examples{
\dontrun{
# read first lines
f <- 'path/to/file.asc'
co <- file(f,encoding='ISO-8859-1')
li <- readLines(con=co,n=30)
close(co)

# roughly estimate the number of lines
n <- floor(file.info(f)$size/(nchar(li[30])+2))

# check header
h <- bafuHeaderTxt(li=li)

# check series
m1 <- bafuSeriesTxt(file=f,nlines=n,skip=h$skip,
                    check.na=T,series.only=F)

# fetch series
m2 <- bafuSeriesTxt(file=f,nlines=n,skip=h$skip,
                    check.na=F,series.only=T)
}
}