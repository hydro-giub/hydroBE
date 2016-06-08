\name{estimateBaseflow}
\alias{estimateBaseflow}

\title{
Estimate Baseflow 
}

\description{
The Baseflow series is estimated with respect to a given window width and ratio to define local minimas.
}

\usage{
estimateBaseflow(y = NULL, win = 5, r = 0.9, 
                 plot = FALSE, use.spline = FALSE, ...)
}

\arguments{
\item{y}{numerical vector, a strictly regular streamflow series (usually on a daily basis)}
\item{win}{integer, block width in number of time units to find local minimas}
\item{r}{number, often around 0.9. Denotes the ratio for constraining baseflow ordinates out of the local minimas.}
\item{plot}{logical, plot runoff and corresponding baseflow series?}
\item{use.spline}{logical, interpolate base flow ordinates by spline smoothing?}
\item{...}{additional arguments passed to \code{\link{spline}}}
}

\details{
The series will be truncated to the last \code{length(y)\%\%win} entries in \code{y} to match whole multiples of \code{win}.
A baseflow ordinate x_{n} is defined by 
\deqn{x_{n-1} > x_{n}*r < x_{n+1}}
where x is a local minima in a window with length \code{win}. Missing values do not satisfy this inequality (see `Examples`).
}

\value{
Numerical vector with estimated baseflow values.
}

\references{
Gustard, A., A. Bullock, and J.M. Dixon (1992). "Low flow estimation in the United Kingdom". pp 20-25.
}

\author{
Simon
}

\note{
Spline smoothing provides nice curves, but does not have any physical validity in the present context of baseflow estimation (though one can argue that simple linear interpolation might be questionable as well). Package \bold{\href{https://cran.r-project.org/package=EcoHydRology}{EcoHydRology}} provides a different approach for baseflow estimation.
}

\examples{
# the last two entries in y get truncated
y <- rnorm(202)
b <- estimateBaseflow(y=y,win=5,r=0.9,plot=TRUE)

# handling of NAs
y[sample(1:200,2)] <- NA
b <- estimateBaseflow(y=y,win=5,r=0.9,plot=TRUE)

# using spline interpolation
n <- 400
f <- c(10,8,6,3,2,1,0.1,0.01)
p <- exp(rnorm(n))
y1 <- filter(p,filter=f/sum(f),sides=1)
y2 <- sin(seq(0,4*pi,length.out=n))+1 
y <- y1+y2
b <- estimateBaseflow(y=y,use.spline=TRUE,r=0.8,plot=TRUE)

# estimate the base flow index (BFI)
sum(b,na.rm=TRUE)/sum(y[!is.na(b)])
}