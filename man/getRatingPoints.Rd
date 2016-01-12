\name{getRatingPoints}
\alias{getRatingPoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate discharge according the Strickler formula 
}
\description{
For a given open channel profil (defined by corresponding points in \code{z} and \code{x}), slope \code{m}, and Strickler value(s) \code{k}, streamflow velocity is calculated 
for given water levels \code{l} according the Strickler formula 

\deqn{v = k*R^{2/3}*m^{1/2}}

where \eqn{R = (area)/(radius)}. \code{k} gets linearly interpolated.
}
\usage{
getRatingPoints(z, x, m, k, baseRef = NULL, l, ni = 1000, plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{numerical vector. Positive depth [m]. It is assumed that the first and last points in z are zero and thus define a theoretical water table with respect to a 
  closed profil.}
  \item{x}{numerical vector. Points along the channel profil where \code{z} was measured [m]. \code{x} has to be nonstrictly increasing, i. e. \eqn{x(n+1) \ge x(n)} for all n.}
  \item{m}{single number. Slope of the river bed perpendicular to the channel profil. \eqn{m = dz/dx = tan(alpha)}}
  \item{k}{single number or numerical vector. Defines the Strickler value(s) [m^(1/3)/s]. In case of a vector, \code{k} should have same dimension as \code{z} and \code{x}.}
  \item{baseRef}{integer(s). Index of reference point(s) in x for water levels \code{l}.}
  \item{l}{numerical vector. Positive water levels [m] with respect to \code{baseRef} for which corresponding discharge should be calculated.}
  \item{ni}{single number. Defines the number of points for interpolation (should be \eqn{\ge 1000}).}
  \item{plot}{logical. Plot the channel profil?}
}
\details{
If \code{baseRef} is not defined, the reference point for water levels \code{l} is defined as the point in \code{x} where \code{z} reaches its maximum. Note that it is allowed
to define the reference point with respect to several points in \code{x}, what might be helpfull in case of a "rugged" river bed (e.g. in case of sand, gravel, and stones).
}
\value{
Matrix with columns water 'level', 'area' below water table, wet 'radius', interpolated 'k', streamflow 'velocity', and 'discharge' [m3/s].
}
\references{
Strickler, A. (1924). "Beitraege zur Frage der Geschwindigkeitsformel und der Rauigkeitszahlen fuer Stroeme, Kanaele und geschlossene Leitungen". Schweizerische Bauzeitung, Heft 23.
}
\author{
Simon
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
z <- c(0,2,2,0)
x <- c(1,1,2,2)
m <- 0.1
k <- 30
baseRef <- 2:3
ni <- 10000
l <- c(0,0.2,0.4,0.6,0.8,1)
pq <- getRatingPoints(z=z,x=x,m=m,k=k,baseRef=baseRef,ni=ni,l=l,plot=TRUE)
print(pq)
plot(x=pq[,'level'],y=pq[,'discharge'])

# trapezoid
z <- c(0,2,2,0)
x <- c(1,3,4,6)
m <- 0.1
k <- 30
baseRef <- 2
ni <- 10000
l <- c(0,0.2,0.4,0.6,0.8,1)
pq <- getRatingPoints(z=z,x=x,m=m,k=k,baseRef=baseRef,ni=ni,l=l,plot=TRUE)
print(pq)
plot(x=pq[,'level'],y=pq[,'discharge'])

# natural channel
z <- c(0,0.1,0.2,0.1,0.23,0.35,0.55,0.6,0.63,0.59,0.43,0.2,0.5,0)
x <- 1:length(z)
m <- 0.1
k <- c(5,10,10,20,50,100,110,105,110,100,60,30,20,10)
baseRef <- 8:10
ni <- 10000
l <- c(0,0.2,0.4,0.6,0.8,1)
l <- seq(0,1,by=0.2)
pq <- getRatingPoints(z=z,x=x,m=m,k=k,baseRef=baseRef,ni=ni,l=l,plot=TRUE)
print(pq)
plot(x=pq[,'level'],y=pq[,'discharge'])
}