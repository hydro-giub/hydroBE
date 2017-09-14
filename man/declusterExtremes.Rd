\name{declusterExtremes}
\alias{declusterExtremes}

\title{
  Decluster maxima in a time series
}

\description{
  Recursively declusters the maxima in a time series based on a minimal
  temporal distance. This can be useful to define independent events for
  an extreme value analysis.
}

\usage{
declusterExtremes(y, L = 11)
}

\arguments{
  \item{y}{numeric vector. A (strictly regular) series; \code{NA}s are
    allowed and will be treated as \eqn{-\infty}.}
  \item{L}{integer. Minimum distance in number of time units.}
}

\details{
  Plateaus are strictly handled as non-maxima. To decluster minimas,
  use \eqn{-1 \cdot y}{-1*y}.
}

\value{
  Integer vector with indices of the maxima within \code{y} with respect
  to \code{L}.
}

\author{
  Simon S
}

\examples{
## use case for an extreme value analysis on a daily basis
r <- as.Date(c('2006-01-01','2010-12-31'))
d <- seq.Date(r[1],r[2],by='day')
y <- rnorm(length(d))

i <- declusterExtremes(y)
g <- format(d[i],'\%Y')
plot(x=d,y=y,type='l')
points(x=d[i],y=y[i],col=3)

## yearly blockmaxima
f <- function(x){
    i <- rep(FALSE,length(x))
    i[which.max(x)] <- TRUE
    return(i)
}
j <- unlist(tapply(y[i],g,f),use.names=FALSE)
i.block <- i[j]

plot(x=d,y=y,type='l')
points(x=d[i],y=y[i],col=3)
points(x=d[i.block],y=y[i.block],col=2,cex=1.5)
abline(v=seq.Date(min(d),max(d)+1,by='year'),lty=3)

## points over threshold
thr <- quantile(y,probs=0.95)
i.pot <- i[y[i]>thr]

plot(x=d,y=y,type='l')
points(x=d[i],y=y[i],col=3)
points(x=d[i.pot],y=y[i.pot],col=2,cex=1.5)
abline(h=thr,lty=3)

\dontrun{
library(ismev)
m1 <- gev.fit(y[i.block])
m2 <- gpd.fit(y[i],threshold=thr,npy=mean(table(g)))}
}