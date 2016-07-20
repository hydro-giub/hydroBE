\name{testLinearTrendPermutation}
\alias{testLinearTrendPermutation}

\title{
Test a time series for a linear trend
}
\description{
The estimated slope of a linear trend within a time series is tested for significance according a permutation test (shuffling).
}
\usage{
testLinearTrendPermutation(x, nper = 1000, per.slopes = F)
}

\arguments{
  \item{x}{numeric vector; missing values are not allowed and it is assumed that the frequency, at which the observation took place, is constant.}
  \item{nper}{integer value; number of permutations, should be >= 1000}
  \item{per.slopes}{boolean; return the slopes as obtained from the permutation procedure?}
}

\details{
The null hypothesis H0 is stated as "no trend"; please note that the test does not account for autocorrelation.
}

\value{
List with entries
\item{slope}{numeric value; the estimated regression coefficient for \code{x}}
\item{p.value}{numeric value; the probability of observing a larger value (in absolute terms) then \code{slope} according to the empiricial distribution of \code{per.slopes}}
\item{per.slopes}{numeric vector of length \code{nper}; the slopes as obtained during the permutation procedure}
}

\references{
Kundzewicz, Z. W., and Robson, A. (Editors) (2000). "Detecting trend and other changes in hydrological data". World Climate Program - Water, WMO/UNESCO, WCDMP-45, WMO/TD 1013, Geneva, 157 pp. 
}

\author{
Original code provided by Pascal H, adapted by Simon S.
}

\seealso{
\code{\link{testBreakpointPettitt}} for a test concerning an abrupte change in the mean level.
}

\examples{
x <- rnorm(100) + seq(0,5,length.out=100)
y <- testLinearTrendPermutation(x=x,nper=1000,per.slopes=TRUE)
par(mfrow=c(1,2))
plot(x,main=paste('slope:',round(y$slope,3)))
hist(y$per.slopes)
}