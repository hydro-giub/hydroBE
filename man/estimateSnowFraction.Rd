\name{estimateSnowFraction}
\alias{estimateSnowFraction}

\title{
Estimate snow fraction
}

\description{
The fraction of frozen water of (potential) precipitation is estimated
by using a temperature threshold. The function can be useful e.g. to
estimate the number of snow days or to accumulate the snow water
equivalent.
}

\usage{
estimateSnowFraction(y, thr = 0.9, trans = 1)
}

\arguments{
\item{y}{numeric vector, matrix, or array. Contains the temperature
  values. See the 'Examples' for how to deal with objects from package
  \bold{\href{https://cran.r-project.org/package=raster}{raster}}.} 
\item{thr}{real number. Temperatures below this threshold value
  convert rain to snow.}
\item{trans}{positive real number. Defines the transition range (see
  'Details').}
}

\details{
To allow for fractional water freezing, \code{thr} gets extended by
\code{trans}. Thus \deqn{y < thr - trans} will convert precipitation
into snow, whereas \deqn{y > thr + trans} preserves precipitation as
rain. For the range spanned by \code{trans} linear interpolation takes
place.

The default values for \code{thr} and \code{trans} are derived from a
hydrological analysis in the Thur catchment (Switzerland) and refer to
temperature in degree Celsius.
}

\value{
Numeric array with the same dimensions as \code{y}, giving the snow
fraction as a number in \eqn{[0,1]}.
}

\author{
Original code provided by Ole R.
}

\examples{
## simulate temperature and precipitation series
x <- seq(0,2*pi,length.out=365)
temp <- 5*sin(x-2)+rnorm(365)
precip <- rgamma(365,shape=2,rate=0.1)
precip[sample(x=1:365,size=250)] <- 0

## separate snow from precipitation
thr <- 0
trans <- 1
f <- estimateSnowFraction(y=temp,thr=thr,trans=trans)
snow <- precip*f
rain <- precip-snow

## plot series
par(mfrow=c(3,1),mar=c(0.5,4,0.5,1),oma=c(4,0,0,0))
plot(temp,type='l',ylab='degree Celsius',xaxt='n')
abline(h=c(thr,thr-trans,thr+trans),lty=3)
plot(precip,type='s',ylab='mm',xaxt='n',col='red')
par(new=TRUE)
plot(snow,type='s',xlab=NA,ylab=NA,axes=FALSE,col='blue')
plot(f,type='l',ylab='snow fraction',xlab='time',xpd=NA)

\dontrun{
## how to deal with temperature fields packed in a RasterBrick
library(raster)
x <- 5 # grid cells in longitude
y <- 5 # grid cells in latitude
z <- 2 # number of time steps
temp <- brick(array(rnorm(x*y*z,sd=4),dim=c(x,y,z)))
f <- estimateSnowFraction(y=as.array(temp))

## make f a RasterBrick with the same attributes as temp
e <- extent(temp)
f <- brick(f,xmn=e[1],xmx=e[2],ymn=e[3],ymx=e[4],crs=proj4string(temp))}
}
