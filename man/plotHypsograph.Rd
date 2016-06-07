\name{plotHypsograph}
\alias{plotHypsograph}

\title{
Plot hypsography against area
}

\description{
Plots the hypsography of a given \code{\link[=Raster-class]{RasterLayer}} against area.
}

\usage{
plotHypsograph(x, ylab = 'm.a.s.l.', xlab = 'area km2',
               col = 'blue', unit.conv = 1e+06, ...)
}

\arguments{
\item{x}{object of class \code{\link[=Raster-class]{RasterLayer}}}
\item{ylab}{y axis label}
\item{xlab}{x axis label}
\item{col}{line color}
\item{unit.conv}{number, conversion factor to link the resolution of \code{x} to \code{xlab}}
\item{\dots}{additional arguments passed to \code{\link{plot}}}
}

\author{
Jan
}

\examples{
library(raster)
x <- raster(matrix(c(rnorm(90),rep(NA,10)),ncol=10,nrow=10),xmx=10,ymx=10)
plotHypsograph(x=x)
}