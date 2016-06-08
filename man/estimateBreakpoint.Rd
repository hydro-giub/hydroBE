\name{estimateBreakpoint}
\alias{estimateBreakpoint}

\title{
Detect a breakpoint within a time series 
}

\description{
The detection of a (single) breakpoint follows the test according Pettitt (1979).
}

\usage{
estimateBreakpoint(x, alpha)
}

\arguments{
\item{x}{matrix, with a time index (e.g. years) in the first column and corresponding observations in the second column}
\item{alpha}{numeric value, significance level}
}

\value{
List with entries
\item{Xk}{matrix}
\item{XEa}{time index of most likely breakpoint}
\item{XE}{no clue}
\item{Xksign}{test statistic}
\item{sig}{logical, test statistic significant according alpha?}
\item{pval}{p-value}
}

\references{
Pettitt, A. N. (1979): A non-parametric approach to the change-point problem. Applied Statistics, 28, 2, 126-135. 
}

\author{
Pascal H
}

\examples{
cand <- c(rnorm(30,2,0.15),rnorm(10,3,0.15))
cand <- cbind(1941:1980,cand) 

res <- estimateBreakpoint(cand, 0.05)

par(mfrow = c(2,1))
plot(cand, type = 'l', xlab='time', ylab='Value')
plot(res$Xk[,'YYYY'], res$Xk[,'Xk'], type = 'l', xlab='time', ylab='Xk')
abline(h=res$Xksign, lty='dashed')
abline(h=-res$Xksign, lty='dashed')
abline(v=res$XEa, col='red')
}