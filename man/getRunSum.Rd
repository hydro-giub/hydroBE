\name{getRunSum}
\alias{getRunSum}

\title{
  Get running sum
}

\description{
  Calculates a running sum with respect to a user specified window width
}

\usage{
getRunSum(x, w)
}

\arguments{
  \item{x}{numeric vector. The vector for which to calculate the running
    sum.}
  \item{w}{integer. Window width in number of time steps.}
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
x <- rnorm(100)
w <- 10
rs <- getRunSum(x=x,w=w)
}
