\name{plot2dProbability}
\alias{plot2dProbability}

\title{
  Plot contours of constant probability of bivariate observations
}

\description{
  The probability density is first estimated using the bivariate normal
  kernel \code{\link{kde2d}} from package MASS. Contours of constant
  probability are then drawn on the scatterplot with the observations.
}

\usage{
plot2dProbability(x1, x2, n = 100, p = 0.9, obs.only = FALSE,
                  bandwith, ...)
}

\arguments{
  \item{x1}{numeric vector. The first variable.}
  \item{x2}{numeric vector. The second variable.}
  \item{n}{integer or integer vector of length two. Defines the number of
    grid points to discretise the plane with the observations. Passed to
    \code{\link{kde2d}}.}
  \item{p}{numeric vector. The probabilities that the contours should
    delineate. E.g. for \code{p=0.9} the contours encompass the smallest
    region such that the probability of a new observation falling in
    this region equals 0.9.}
  \item{obs.only}{logical. Integrate the complete pdf over the domain of
    the observations or only at the location of the observations?}
  \item{bandwith}{real number or vector of length two. Defines the
    bandwith for the normal kernel. Passed to \code{\link{kde2d}}.}
  \item{\dots}{additional arguments. Passed to \code{\link{plot.default}}.}
}

\details{
  Missing values in \code{x1} and \code{x2} are not allowed. If
  \code{obs.only=TRUE} the kernel density is sampled at the location of
  the points defined by \code{x1} and \code{x2}. The quantiles of these
  density values (with probability \eqn{1-p}) are then used to
  draw the contours. For  \code{obs.only=FALSE} the complete density
  function is integrated. 
}

\references{
  Haenggi, P., and R. Weingartner (2011).
  "Inter-annual variability of runoff and climate within the Upper Rhine River basin, 1808-2007".
  Hydrological Sciences Journal.
}

\author{
  Original code provided by Pascal H.
}

\seealso{
  \code{\link{kde2d}} from package MASS; \code{\link{contour}}
}

\examples{
## two independent Gaussian random variables
x1 <- rnorm(1000,mean=0,sd=1)
x2 <- rnorm(1000,mean=1,sd=2)
plot2dProbability(x1,x2,p=c(0.5,0.7,0.9),pch=19,cex=0.5)

## manually select the bandwith
plot2dProbability(x1,x2,p=c(0.5,0.7,0.9),pch=19,cex=0.5,bandwith=5)
plot2dProbability(x1,x2,p=c(0.5,0.7,0.9),pch=19,cex=0.5,bandwith=0.05)

## a non-normal distribution
x1 <- c(runif(500,0,1),runif(500,1,5))
x2 <- c(runif(500,0,5),runif(500,4,5))
plot2dProbability(x1,x2,p=c(0.5,0.7,0.9),pch=19,cex=0.5)
}
