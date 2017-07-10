\name{plotHypsograph}
\alias{plotHypsograph}

\title{
Plot hypsography against area
}

\description{
Plots the hypsography of a given region against the cumulated area.
}

\usage{
plotHypsograph(x, ylab = "m a.s.l.", x.res = 1000, y.res = 1000, ...)
}

\arguments{
\item{x}{numeric vector or matrix. Contains the elevation values; see
  the 'Examples' for how to deal with objects from package
  \bold{\href{https://cran.r-project.org/package=raster}{raster}}.}
\item{ylab}{string. The y axis label.}
\item{x.res}{real number. The length of a grid cell in the x direction
  with unit [m].}
\item{y.res}{real number. Length of a grid cell in the y direction with
  unit [m].}
\item{\dots}{additional arguments. Passed to \code{\link{plot.default}}}
}

\author{
Jan S
}

\examples{
\dontrun{
library(raster)
dem <- matrix(c(rnorm(90),rep(NA,10)),ncol=10,nrow=10)
x <- raster(dem,xmx=50,ymx=50) 
r <- res(x)
plotHypsograph(x=values(x),x.res=r[1],y.res=r[2],col='blue')}
}