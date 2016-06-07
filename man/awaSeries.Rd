\name{awaSeries}
\alias{awaSeries}
\alias{awaSeriesHydropro}

\title{
Sniff in Awa time series
}

\description{
Helper functions to summarize hydrological time series provided by Awa (Amt fuer Wasser und Abfall Kanton Bern). It is intended to provide a summary useful for the import of Awa files, not a summary in the sense of a statistical description. 
}

\usage{
awaSeriesHydropro(file = NULL, nlines = 0, skip = 3, 
                   check.na = FALSE, series.only = FALSE)
}

\arguments{
\item{file}{string, file name}
\item{nlines}{integer, number of lines in \code{file}}
\item{skip}{integer, number of header lines}
\item{check.na}{logical, check series for missing values? Leading and trailing \code{NA}s will be ignored.}
\item{series.only}{logical, return series in a nx1 matrix instead of a summary? Time stamps will be attached as row names.}
}

\value{
\code{awaSeries} returns a list with entries
\item{res}{string, temporal resolution (either 10 min, hourly, daily, monthly, yearly, irregular)}
\item{start}{string, date of the first non-missing observation}
\item{end}{string, date of the last non-missing observation}
\item{nas}{integer, number of missing values (leading and trailing \code{NA}s will be ignored)}

If \code{series.only=F}, \code{awaSeriesHydropro} additionally returns
\item{type}{string, either "peak" or "mean" value}
\item{sep}{character, field separator in \code{file}}
Otherwise \code{awaSeriesHydropro} returns a nx1 matrix.
}

\section{Warning}{
\enumerate{
\item \code{check.na=T} or \code{series.only=T} requires to read the whole series and thus might be time consuming for larger files.
\item The approach for guessing the temporal resolution of the series might fail when the series is irregular (i.e. when missing values are not declared, but instead the corresponding rows are omitted).
}
}

\author{
Simon
}

\note{
In case of mean values, returned date and time information indicate the start of the corresponding period. E.g. for hourly mean values, "2015.09.01 01:00" depicts the interval 01:00-02:00 and for 10 min mean values "2015.09.01 01:00" depicts the interval 01:00-01:09.
}

\seealso{
\code{\link{awaHeader}} which does the job for the header lines. \code{\link{checkAwaFiles}} and \code{\link{importAwaFiles}} which both rely on \code{awaSeries}.
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
h <- awaHeaderTxt(li=li)

# check series
m1 <- awaSeriesHydropro(file=f,nlines=n,skip=h$skip,
                        check.na=T,series.only=F)

# fetch series
m2 <- awaSeriesHydropro(file=f,nlines=n,skip=h$skip,
                        check.na=F,series.only=T)
}
}