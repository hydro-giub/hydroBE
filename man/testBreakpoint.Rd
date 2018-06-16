\name{testBreakpoint}
\alias{testBreakpoint}

\title{
  Test a time series for a breakpoint in the mean with Pettitt's test
}

\description{
  Following the test of Pettitt (1979), a time series is tested for a
  single, abrupt breakpoint concerning a shift in the mean.
}

\usage{
testBreakpoint(x, alpha = 0.05)
} 

\arguments{
  \item{x}{numeric vector. Missing values are not allowed and it is
    assumed that the frequency, at which the observation took place, is
    constant.}
  \item{alpha}{real number. Defines the significance level alpha.}
}

\details{
  The test by Pettitt (1979) relies on the non-parametric Mann-Whitney
  U-test for assessing whether two samples are drawn from the same
  distribution. The null hypothesis H0 is stated as 'no change in level'
  and the alternative hypothesis HA as 'change in level'; only the
  two-sided test is implemented here.
}

\value{
  List with entries
  \item{i}{integer. The index of the most probable breakpoint in \code{x},
    i.e. \eqn{i=argmax_t(|U_t|)}{i=argmax(t)|U(t)|}.}
  \item{p.value}{real number. The p-value, i.e. the probability of not
    observing a breakpoint at \code{i}.}
  \item{k}{real number. \eqn{k=|U_{i}|}{k=|U(i)|}.}
  \item{critical.value}{real number. The value of \eqn{x} for which
    \eqn{P(K<x)=1-alpha}.}
  \item{reject.HO}{logical. Reject the null hypothesis, i.e.
    \eqn{k \ge critical.value}?}
  \item{u}{numeric vector. \eqn{U_t}{U(t)} corresponding to the values
    in \code{x}.}
}

\references{
  Pettitt, A. N. (1979).
  "A non-parametric approach to the change-point problem".
  Applied Statistics.
}

\author{
  Original code provided by Pascal H.
}

\examples{
x <- c(rnorm(20,2,0.15),rnorm(20,3,0.15))
p <- testBreakpoint(x,alpha=0.05)
par(mfrow = c(2,1))

plot(x,type='l',xlab='time',ylab='observation')
abline(v=p$index,col='red')
plot(abs(p$u),type='l',xlab='time',ylab='abs(u)')
abline(h=p$critical.value,lty=2)
abline(v=p$index,col='red')
}