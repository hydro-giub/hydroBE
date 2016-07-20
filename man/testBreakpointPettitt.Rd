\name{testBreakpointPettitt}
\alias{testBreakpointPettitt}

\title{
Detect a breakpoint in level within a time series 
}

\description{
Following the test of Pettitt (1979), a time series is tested for a single, abrupt change-point concerning a shift in the mean.
}

\usage{
testBreakpointPettitt(x, alpha=0.05)
} 

\arguments{
\item{x}{numeric vector; missing values are not allowed and it is assumed that the frequency, at which the observation took place, is constant.}
\item{alpha}{numeric value, significance level}
}

\details{
The test by Pettitt (1979) relies on the non-parametric Mann-Whitney U-test for assessing whether or not two samples are drawn from the same distribution. The null hypothesis H0 is stated as "no change in level" and the alternative hypothesis HA as "change in level"; only the two-sided test is implemented here.
}

\value{
List with entries
\item{index}{integer; index of the most probable breakpoint in \code{x}, i.e. \code{index}=argmax(|U[t]|)}
\item{p.value}{p-value (i.e. the probability of not observing a breakpoint at \code{index})}
\item{k}{|U[t]| at t=index, i.e. k=max(|U[t]|)}
\item{critical.value}{the value of k for which P(K>k) = \code{alpha}}
\item{reject.HO}{boolean; reject the null hypothesis, i.e. \code{k}>=\code{critical.value}?}
\item{u}{U[t] corresponding to the values in \code{x}}
}

\references{
Pettitt, A. N. (1979). "A non-parametric approach to the change-point problem". Applied Statistics, 28, 2, 126-135. 
}

\author{
Original code provided by Pascal H, adapted by Simon S.
}

\examples{
# jump in the mean
x <- c(rnorm(20,2,0.15),rnorm(20,3,0.15))
p <- testBreakpointPettitt(x,alpha=0.05)
par(mfrow = c(2,1))

plot(x,type='l',xlab='time',ylab='observation')
abline(v=p$index,col='red')
plot(abs(p$u),type='l',xlab='time',ylab='abs(u)')
abline(h=p$critical.value,lty=2)
abline(v=p$index,col='red')

# jump in the variance
x <- c(rnorm(20,0,1),rnorm(20,0,10))
p <- testBreakpointPettitt(x,alpha=0.05)
par(mfrow = c(2,1))

plot(x,type='l',xlab='time',ylab='observation')
abline(v=p$index,col='red')
plot(abs(p$u),type='l',xlab='time',ylab='abs(u)')
abline(h=p$critical.value,lty=2)
abline(v=p$index,col='red')
}