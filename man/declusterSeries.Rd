\name{declusterSeries}
\alias{declusterSeries}

\title{
Decluster local maximas in a time series
}

\description{
Declusters recursively the local maximas in a time series based on a minimal temporal distance.
}

\usage{
declusterSeries(y = NULL, L = NULL)
}

\arguments{
\item{y}{numerical vector. The (strictly regular) series. \code{NA}s are allowed and will be treated as -Inf.}
\item{L}{integer. Minimum distance in number of time units.}
}

\details{
Plateaus are strictly handled as non-maximas, see `Examples`.
}

\value{
Integer vector with indices of the maximas within \code{y} with respect to \code{L}.
}

\author{
Simon
}

\examples{
y <- c(0,0,1,1,0,0,0,3,2,NA,1,5,3,4,5,6,5,2,3,2,4,5,4,3,2,1,0)

# handling of NAs and plateaus
i <- declusterSeries(y=y,L=2)
plot(y,type='l')
points(y,cex=0.5)
points(y=y[i],x=i,col=2)

# decluster minimas
i <- declusterSeries(y=-y,L=2)
plot(y,type='l')
points(y,cex=0.5)
points(y=y[i],x=i,col=2)
}