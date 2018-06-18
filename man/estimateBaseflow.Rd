\name{estimateBaseflow}
\alias{estimateBaseflow}

\title{
  Estimate baseflow 
}

\description{
  Baseflow summarises the runoff contributions from slowly varying
  hydrological storages after the offset of a rain event and can be
  estimated by the interpolation of local minima.
}

\usage{
estimateBaseflow(y, win = 5, r = 0.9, plot = FALSE,
                 use.spline = FALSE, ...)
}

\arguments{
  \item{y}{numeric vector. A strictly regular streamflow series (usually
    on a daily basis).}
  \item{win}{integer. Block width in number of time units to find local
    minimas.}
  \item{r}{real number. Denotes the ratio for constraining baseflow
    ordinates out of the local minimas; often set around 0.9.}
  \item{plot}{logical. Plot runoff and corresponding baseflow series?}
  \item{use.spline}{logical. Interpolate baseflow ordinates by spline
    smoothing?}
  \item{...}{additional arguments. Passed to \code{\link{spline}}.}
}

\details{
  The series will be truncated to the last \code{length(y)\%\%win} entries
  in \code{y} to match whole multiples of \code{win}. A baseflow ordinate
  \eqn{x_{t}}{x(t)} is defined by
  \deqn{x_{t-1} > r \cdot x_{t} < x_{t+1}}{x(t-1) > r*x(t) < x(t+1)}
  where
  \eqn{x} is a local minima in a window with length \code{win}. Missing
  values do not satisfy this inequality (see 'Examples').
}

\value{
  Numeric vector with estimated baseflow values.
}

\references{
  Gustard, A., A. Bullock, and J. M. Dixon (1992).
  "Low flow estimation in the United Kingdom".
  Report No. 108. Institute of Hydrology.
}

\author{
  Simon S
}

\note{
  Package
  \bold{\href{https://cran.r-project.org/package=EcoHydRology}{EcoHydRology}}
  provides a different approach for baseflow estimation.
}

\examples{
## handling of NAs
y <- rnorm(202)
y[sample(1:200,2)] <- NA
b <- estimateBaseflow(y=y,plot=TRUE)

## mimic a streamflow series
n <- 400
f <- c(10,8,6,3,2,1,0.1,0.01)
p <- exp(rnorm(n))
y1 <- filter(p,filter=f/sum(f),sides=1)
y2 <- sin(seq(0,4*pi,length.out=n))+1 
y <- y1+y2

## use spline interpolation
b <- estimateBaseflow(y=y,plot=TRUE,use.spline=TRUE)

## estimate the baseflow index (BFI)
sum(b,na.rm=TRUE)/sum(y[!is.na(b)])
}
