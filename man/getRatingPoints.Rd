\name{getRatingPoints}
\alias{getRatingPoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate discharge according the Strickler formula 
}
\description{
For a given channel profil (defined by corresponding points in \code{z} and \code{x}) and slope \code{m}, discharge is calculated 
for given water levels \code{p} according the Strickler formula.
}
\usage{
getRatingPoints(z, x, m, k, baseRef = NULL, p, ni = 1000, plot = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{numerical vector. Positive depth [m]; it is assumed that the first and last points in z are zero and and thus define a theoretical water table of a closed profil.}
  \item{x}{numerical vector. Points along the channel profil where \code{z} was measured [m]. \code{x} has to be nonstrictly increasing, i. e. \eqn{x(n+1) \ge x(n)} for all n.}
  \item{m}{single number. Slope of the river bed perpendicular to the channel profil. m=dy/dx or m=tan(alpha)}
  \item{k}{single number or numerical vector. Defines the Stricklervalue(s) [m^(1/3)/s]; in case of a vector should have same dimension as \code{z} and \code{x}.}
  \item{baseRef}{integer(s). Index of reference point(s) in x for water levels \code{p}}
  \item{p}{numerical vector. Positive water levels [m] with respect to \code{baseRef} for which we want corresponding discharge.}
  \item{ni}{single number. Defines the number of points for interpolation (should be >=1000).}
  \item{plot}{logical. Plot the channel profil?}
}
\details{
If baseRef is not defined, the reference point for water levels \code{p} is defined as the point in \code{x} where \code{z} reaches its maximum. Note that it is allowed
to define the reference point with respect to several points in \code{x}, what might be helpfull in case of a "rugged" river bed (e.g. in case of sand, gravel, and stones).
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
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
p <- c(0,0.2,0.4,0.6,0.8,1)
pq <- getRatingPoints(z=z,x=x,m=m,k=k,baseRef=baseRef,ni=ni,p=p,plot=T)
print(pq)

# trapezoid
z <- c(0,2,2,0)
x <- c(1,3,4,6)
m <- 0.1
k <- 30
baseRef <- 2
ni <- 10000
p <- c(0,0.2,0.4,0.6,0.8,1)
pq <- getRatingPoints(z=z,x=x,m=m,k=k,baseRef=baseRef,ni=ni,p=p,plot=T)
print(pq)

# "natural" channel
z <- c(0,0.1,0.2,0.1,0.23,0.35,0.55,0.6,0.63,0.59,0.43,0.2,0.5,0)
x <- 1:length(z)
m <- 0.1
k <- c(5,10,10,20,50,100,110,105,110,100,60,30,20,10)
baseRef <- 8:10
ni <- 10000
p <- c(0,0.2,0.4,0.6,0.8,1)
pq <- getRatingPoints(z=z,x=x,m=m,k=k,baseRef=baseRef,ni=ni,p=p,plot=T)
print(pq)
}