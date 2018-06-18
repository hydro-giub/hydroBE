\name{getApi}
\alias{getApi}

\title{
  Calculate the antecedent precipitation index
}

\description{
  The antecedent precipitation index (API) can be used to estimate the
  moisture conditions of the land surface.
}

\usage{
getApi(x, k = 0.9, n = 5, finite = TRUE)
}

\arguments{
  \item{x}{numeric vector. The precipitation time series used to
    calculate the API. It is assumed that the time series has a constant
    frequency and no missing values.}
  \item{k}{real number or numeric vector. The weights to sum up the
    antecedent precipitation values, see 'Details' below. The elements
    of \code{k} should be in \eqn{[0,1]} to make physically sense.}
  \item{n}{integer. The number of antecedent time steps to consider for
    the API.}
  \item{finite}{logical. Use a fixed number of previous days (as
    specified with \code{n}), or set an initial API value and
    recursively calculate the API at time t with the API and
    precipitation from time t-1?}
}

\details{
  \itemize{
    \item  If \code{finite = TRUE} and \code{k} is a single number, the weights
    are derived as \deqn{k^i, i=0,...,n-1}{k^i, i=0,...,n-1}.
    \item If \code{finite = TRUE} and \code{k} is a numeric vector of
    length greater than 1, \code{n} is ignored and \code{k} is used for
    the weighting procedure. In this case, the weights defined in
    \code{k} should increase along the vector indices.
    \item If \code{finite = FALSE}, \code{n} is ignored and \code{k}
    should be a single number. The API series is initialised with
    \code{x[1]} and recursively calculated according to
    \deqn{API_t = k \cdot API_{t-1} + x[t-1]}{API_t = k*API_{t-1} + x[t-1]}
  }
 If \code{k} is a single number, the solutions of \code{finite = TRUE}
 and \code{finite = FALSE} converge for increasing values of \code{n}
 (see 'Examples' below).
}

\value{
  Numeric vector, i.e. the API time series.
}

\references{
  Kohler, M. A., and R. K. Linsley (1924).
  "Predicting the runoff from storm rainfall".
  National Oceanic and Atmospheric Administration Weather Bureau Research Papers No. 34.
}

\author{
 Simon S
}

\examples{
## the larger n, the closer are the solutions 
x <- rexp(1000)
api1 <- getApi(x=x)
api2 <- getApi(x=x,finite=FALSE)

x <- rexp(1000)
api3 <- getApi(x=x,n=300)
api4 <- getApi(x=x,finite=FALSE)

par(mfcol=c(2,1))
plot(x=api1,y=api2)
abline(a=0,b=1,col=2)
plot(x=api3,y=api4)
abline(a=0,b=1,col=2)

## user defined weights
x <- rexp(1000)
k <- 1/(15:1)
api <- getApi(x=x,k=k)
}