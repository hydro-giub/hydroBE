\name{getRatingPoints}
\alias{getRatingPoints}

\title{
Calculate discharge according to the Strickler formula 
}

\description{
For a given open channel profile defined by \code{z} in the vertical,
\code{x} in the horizontal, slope \code{m}, and Strickler value 
\code{k}, streamflow velocity is calculated for the water levels
\code{l} according to the Strickler formula
\deqn{v = k \cdot R^{2/3} \cdot m^{1/2}}{v = k*R^(2/3)*m^(1/2)}
with hydraulic radius \eqn{R = area/radius}.
}

\usage{
getRatingPoints(z, x, k, m, l, plot = FALSE)
}

\arguments{
\item{z}{numeric vector. Altitude [m] with respect to e.g. sea level
  or any other reference point.}
\item{x}{numeric vector. Points along the channel profile where
  \code{z} was measured [m].}
\item{k}{real number or numeric vector. Defines the Strickler
  value(s) [m^(1/3)/s]. In case of a vector, \code{k} should have the
  same length as \code{z} and \code{x} and gets linearly interpolated.}
\item{m}{real number. Slope of the river bed perpendicular to the
  channel profile, \eqn{m = tan(\alpha)}.}
\item{l}{numeric vector. Positive water levels [m] for which
  corresponding discharge should be calculated.}
\item{plot}{logical. Plot the channel profile?}
}

\details{
The reference point for water levels \code{l} is defined as the point in
\code{x} where \code{z} reaches its minimum.
}

\value{
Numeric matrix with columns water 'level' [m], profile 'area' below the
water table [m^2], wet profile 'radius' [m], interpolated 'k',
streamflow 'velocity' [m/s], and 'discharge' [m^3/s].
}

\note{
The Strickler formula is also known as Manning or Gauckler formula.
}

\references{
Strickler, A. (1924).
"Beitraege zur Frage der Geschwindigkeitsformel und der Rauigkeitszahlen fuer Stroeme, Kanaele und geschlossene Leitungen".
Schweizerische Bauzeitung, Heft 23.
}

\author{
Original code provided by Alain B.
}

\examples{
## quadratic profile
z <- c(10,0,0,10)
x <- c(0,0,10,10)
k <- sample(0:100,size=length(z))
m <- 0.03
l <- 1:10
pq <- getRatingPoints(z=z,x=x,k=k,m=m,l=l,plot=TRUE)
print(pq)
plot(x=pq[,'level'],y=pq[,'discharge'])

## trapezoid
z <- c(2,0,0,2)
x <- c(1,3,4,6)
k <- 30
m <- 0.1
l <- seq(0,1.5,by=0.2)
pq <- getRatingPoints(z=z,x=x,k=k,m=m,l=l,plot=TRUE)
print(pq)
plot(x=pq[,'level'],y=pq[,'discharge'])

## natural channel
z <- c(9,9,9,10,9,8,7,6,7,8,7,6,5,4,3,4,5,6,5,4,1,4,5,6,7,8,9,8,7,8,8)
x <- c(1:25,23:28)
k <- sample(0:100,size=length(z))
m <- 0.03
l <- 1:13
pq <- getRatingPoints(z=z,x=x,k=k,m=m,l=l,plot=TRUE)
print(pq)
plot(x=pq[,'level'],y=pq[,'discharge'])
}