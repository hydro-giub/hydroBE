\name{checkFiles}
\alias{checkFiles}
\alias{checkSeries}
\alias{getTimestamps}

\title{
Helper functions to sniff in or import from files
}

\description{
These functions work behind the scene of \code{bafuFiles} and the like.
}

\usage{
checkSeries(li = NULL, check.na = FALSE, sblock = NULL)
getTimestamps(res = NULL, sm = NULL)
}

\arguments{
\item{li}{character vector, see 'Details'}
\item{check.na}{logical, check series for missing values? Leading and trailing \code{NA}s will be ignored.}
\item{sblock}{integer, use \code{li[1:sblock]} to check the temporal resolution}
\item{res}{string, temporal resolution. One of "year", "month", "day", "hour", or "10 min".}
\item{sm}{character matrix as returned by e.g. \code{\link{checkBafuFiles}}}
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

\code{getTimestamps} returns a list with entries
\item{x}{character vector with timestamps}
\item{n}{integer, number of relevant characters to get the corresponding timestamps out of "YYYY.MM:DD HH:MM"}
}

\author{
Simon S
}

\note{
These functions are for internal purposes only and might be subject to changes in the near future.
}