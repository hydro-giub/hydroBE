\name{testLinearTrend}
\alias{testLinearTrend}

\title{
  Test a time series for a linear trend with a permutation test
}

\description{
  The estimated slope of a linear trend within a time series is tested for
  significance with a permutation test (shuffling).
}

\usage{
testLinearTrend(x, n = 1000, ret.slopes = F)
}

\arguments{
  \item{x}{numeric vector. Missing values are not allowed and it is
    assumed that the frequency, at which the observation took place, is
    constant.}
  \item{n}{integer. Number of permutations, should hold \eqn{n \ge 1000}.}
  \item{ret.slopes}{logical. Return the slopes as obtained from the
    permutation procedure?}
}

\details{
  The null hypothesis H0 is stated as 'no trend present' and the
  alternative hypothesis HA as 'trend present'. Please note that the test
  does not account for autocorrelation.
}

\value{
  List with entries
  \item{b}{real number. The coefficient when \code{x} is regressed
    against time.}
  \item{p.value}{real number. The probability of observing a stronger
    trend than \code{b} under H0. If \eqn{b<0} then \code{p.value} amounts
    to \eqn{P(B<b)} and \eqn{P(B>b)} otherwise.}
  \item{slopes}{numeric vector of length \code{n}. The slopes as
    obtained from the permutation procedure.}
}

\references{
  Kundzewicz, Z. W., and Robson, A. (Editors) (2000).
  "Detecting trend and other changes in hydrological data".
  World Climate Program - Water, WMO/UNESCO, WCDMP-45, WMO/TD 1013,
  Geneva, 157 pp.
}

\author{
  Original code provided by Pascal H.
}

\examples{
x <- rnorm(100) + seq(0,5,length.out=100)
y <- testLinearTrend(x=x,n=1000,ret.slopes=TRUE)
par(mfrow=c(1,2))
plot(x,main=paste('slope:',round(y$b,3)))
hist(y$slopes)
}