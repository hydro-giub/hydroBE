\name{getRatingPoints}
\alias{getRatingPoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate discharge according the Strickler formula 
}
\description{
For a given open channel profile (defined by corresponding points in \code{z} and \code{x}), slope \code{m}, and Strickler value(s) \code{k}, streamflow velocity is calculated 
for given water levels \code{l} according the Strickler formula 

\deqn{v = k*R^{2/3}*m^{1/2}}

where hydraulic radius \eqn{R = (area)/(radius)}. \code{k} gets linearly interpolated.
}
\usage{
getRatingPoints(z, x, k, m, l, plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{numerical vector. Altitude [m] with respect to e.g. sea level or any other reference point.}
  \item{x}{numerical vector. Points along the channel profile where \code{z} was measured [m].}
  \item{k}{single number or numerical vector. Defines the Strickler value(s) [m^(1/3)/s]. In case of a vector, \code{k} should have same dimension as \code{z} and \code{x}.}
  \item{m}{single number. Slope of the river bed perpendicular to the channel profile. \eqn{m = dz/dx = tan(alpha)}}
  \item{l}{numerical vector. Positive water levels [m] for which corresponding discharge should be calculated.}
  \item{plot}{logical. Plot the channel profile?}
}
\details{
The reference point for water levels \code{l} is defined as the point in \code{x} where \code{z} reaches its minimum.
}
\value{
Matrix with columns water 'level' [m], profile 'area' [m2] below water table, wet profile 'radius' [m], interpolated 'k', streamflow 'velocity' [m/s], and 'discharge' [m3/s].
}
\references{
Strickler, A. (1924). "Beitraege zur Frage der Geschwindigkeitsformel und der Rauigkeitszahlen fuer Stroeme, Kanaele und geschlossene Leitungen". Schweizerische Bauzeitung, Heft 23.
}
\author{
Original code provided by Alain Bruehlmann.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# quadratic profile
z <- c(10,0,0,10)
x <- c(0,0,10,10)
k <- sample(0:100,size=length(z))
m <- 0.03
l <- 1:10
pq <- getRatingPoints(z=z,x=x,k=k,m=m,l=l,plot=TRUE)
print(pq)
plot(x=pq[,'level'],y=pq[,'discharge'])

# trapezoid
z <- c(2,0,0,2)
x <- c(1,3,4,6)
k <- 30
m <- 0.1
l <- seq(0,1.5,by=0.2)
pq <- getRatingPoints(z=z,x=x,k=k,m=m,l=l,plot=TRUE)
print(pq)
plot(x=pq[,'level'],y=pq[,'discharge'])

# natural channel
z <- c(9,9,9,10,9,8,7,6,7,8,7,6,5,4,3,4,5,6,5,4,1,4,5,6,7,8,9,8,7,8,8)
x <- c(1:25,23,24,25,26,27,28)
k <- sample(0:100,size=length(z))
m <- 0.03
l <- 1:13
pq <- getRatingPoints(z=z,x=x,k=k,m=m,l=l,plot=TRUE)
print(pq)
plot(x=pq[,'level'],y=pq[,'discharge'])
}