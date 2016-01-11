\name{estimateBFI}
\alias{estimateBFI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate Base Flow Index (BFI)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
estimateBFI(y = NULL, win = 5, r = 0.9, plot = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{\code{vector}, strictly regular runoff series (usually on a daily basis)}
  \item{win}{\code{integer}, block width in number of time units to find local minimas}
  \item{r}{number bewtween 0 and 1, denoting the ratio for constraining baseflow ordinates out of the local minimas}
  \item{plot}{\code{logical}, plot runoff and corresponding baseflow series?}
}
\details{
The series will be truncated to the last \code{length(y)\%\%win} entries in \code{y} to match whole multiples of \code{win}
}
\value{
BFI
}
\references{
Gustard, A., A. Bullock, and J.M. Dixon (1992). "Low flow estimation in the United Kingdom". pp 20-25.
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
y <- rnorm(502)
y[sample(1:500,5)] <- NA
bfi <- estimateBFI(y=y,win=5,r=0.9,plot=F)