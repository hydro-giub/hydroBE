\name{estimateSnowFraction}
\alias{estimateSnowFraction}

\title{
Estimate snow fraction
}

\description{
For a given precipitation and temperature time series the fraction of frozen water (some people call it snow) is estimated.
}

\usage{
estimateSnowFraction(precip, temp, threshold = 0.9, transition = 1)
}

\arguments{
\item{precip}{numerical vector with precipitation values}
\item{temp}{numerical vector with temperature values}
\item{threshold}{numerical value, temperatures below this value convert rain to snow}
\item{transition}{numerical value, transition range (see `Details`)}
}

\details{
To allow for fractional water freezing, \code{threshold} gets extended by +/- \code{transition}. Thus
\deqn{temp < threshold - transition}
will convert precipitation into snow, whereas
\deqn{temp > threshold + transition}
preserves precipitation as rain. For the range spanned by \code{transition} linear interpolation takes place.

The default values for \code{threshold} and \code{transition} are derived from a hydrological analysis in the Thur catchment, Switzerland.
}

\value{
Numerical vector with the same length as \code{precip} and \code{temp} giving the snow fraction as a number in \code{[0,1].}
}

\author{
Ole R
}

\examples{
x <- seq(0,2*pi,length.out=365)
temp <- 5*sin(x-2)+rnorm(365)
precip <-  rgamma(365,shape=2,rate=0.1)
precip[sample(x=1:365,size=200)] <- 0
f <- estimateSnowFraction(precip=precip,temp=temp)

plot(temp,type='l',ylab='degree Celsius',xlab='time')
abline(h=c())
par(new=TRUE)
ylim <- range(precip)
plot(precip,type='s',xlab=NA,ylab=NA,axes=FALSE,col='red',ylim=ylim)
axis(4)
mtext('mm',side=4)
par(new=TRUE)
plot(precip*f,type='s',xlab=NA,ylab=NA,axes=FALSE,col='blue',ylim=ylim)
}