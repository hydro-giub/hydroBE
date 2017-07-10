\name{applySnht}
\alias{applySnht}

\title{
Conduct the Standard Normal Homogeneity Test
}

\description{
The Standard Normal Homogeneity Test (SNHT) tries to detect a breakpoint
concerning the mean value of a time series, usually in combination with
a homogeneous reference series. If so, the goal is to detect artifical
instationarities introduced by e.g. station relocations or sensor
drifts. 
}

\usage{
applySnht(y, x, method, omit = 0)
}

\arguments{
\item{y}{numeric vector. The candidate series to be tested.}
\item{x}{numeric vector. The reference series (often this series is a
  composite of nearby observation sites, e.g. from weather stations).}
\item{method}{string. Must be either 'ratio' or 'diff' to specify
  whether to take ratios or differences between the candidate and
  reference series.}
\item{omit}{integer. Number of leading and trailing observations to omit
  when estimating the most probable time point of change.}
}

\details{
If \code{x} is missing, the test is conducted on the candidate series
\code{y} only. Since the SNHT is known to be overly sensitive to detect
breakpoints at the beginning and end of the series, the argument
\code{omit} can be used to exclude the first and last few time steps
while extracting the maximum of the test statistic.
}

\value{
List with entries:
\item{i}{integer. The index of the most probable breakpoint, i.e. the
  last time step before the break occures.}
\item{test}{logical vector. Significance of the most probable breakpoint
  \code{i} with respect to the levels 0.1, 0.08, 0.06, 0.05, 0.025, and
  0.01. The critical values are taken from Khaliq and Ouarda (2007).}
\item{tv}{numeric vector. The test statistic.}
\item{q}{numeric vector. The q series according to \code{method}; if
  \code{x} is missing, \code{q} equals \code{y} with mean zero and standard
  deviation one.}
}

\references{
Alexanderson, H. (1986).
"A homogeneity test applied to precipitation data".
International Journal of Climatology.

Khaliq, M. N., and T. B. M. J. Ouarda (2007).
"On the critical values of the standard normal homogeneity test (SNHT)".
International Journal of Climatology.
}

\author{
Original code provided by Pascal H.
}

\seealso{
\code{\link{testLinearTrend}} and \code{\link{testBreakpoint}} for other
statistical tests concerning the detection of instationarities in time
series (but without the option to include a reference series).
}

\examples{
n <- 40
x <- c(rnorm(n/2,2,0.15),rnorm(n/2,2.5,0.15))
y <- rnorm(n,2,0.15)
snht <- applySnht(x=x,y=y,method='diff',omit=2)
i <- snht$i
plot(x,type='l')
abline(v=i+0.5,lty=3)
lines(x=c(1,i),y=rep(mean(x[1:i]),2),col=2)
lines(x=c(i+1,n),y=rep(mean(x[(i+1):n]),2),col=2)
}