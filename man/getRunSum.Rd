\name{getRunSum}
\alias{getRunSum}

\title{
  Get running sum
}

\description{
  Calculates a running sum with respect to a user specified window width.
}

\usage{
getRunSum(x, w)
}

\arguments{
  \item{x}{numeric vector. The vector for which to calculate the running
    sum.}
  \item{w}{integer. Window width in number of time steps.}
}

\details{
  The function implements a special case of
  \code{\link{filter}}, i.e. the case where the filter consists of 1s
  only, see 'Examples' below.
}

\value{
  Numeric vector of the same length as \code{x}.
}

\author{
 Simon S.
}

\seealso{
  \code{\link{filter}}
}

\examples{
x <- rnorm(20)
w <- 4
getRunSum(x=x,w=w)
filter(x=x,filter=rep(1,times=w),sides=1)
}
