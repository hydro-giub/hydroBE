\name{checkFiles}
\alias{checkFiles}
\alias{checkSeries}

\title{
Helper functions to sniff in or import from files
}

\description{
These functions work behind the scene of \code{bafuFiles} and the like.
}

\usage{
checkSeries(li = NULL, check.na = FALSE, sblock = NULL)
}

\arguments{
\item{li}{character vector, see 'Details'}
\item{check.na}{logical, check series for missing values? Leading and trailing \code{NA}s will be ignored.}
\item{sblock}{integer, use \code{li[1:sblock]} to check the temporal resolution}
}

\details{
\code{checkSeries} expects a character vector with entries of the form "2015.09.31 01:00;3.456" as input for argument \code{li} where missing values are indicated by "NA". It then checks the temporal resolution and also tests for missing values, if required.
}

\value{
\code{checkSeries} returns a list with entries
\item{res}{string, temporal resolution (either 10 min, hourly, daily, monthly, yearly, irregular)}
\item{start}{string, date of the first non-missing observation}
\item{end}{string, date of the last non-missing observation}
\item{nas}{integer, number of missing values (leading and trailing \code{NA}s will be ignored)}
}

\author{
Simon
}

\note{
These functions are likely to be changed in the near future.
}

\examples{
# to do
}