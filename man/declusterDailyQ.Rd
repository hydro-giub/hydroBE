\name{declusterDailyQ}
\alias{declusterDailyQ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
here we need a title in order to check and install the package
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
declusterDailyQ(x = NULL, L = 11)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{L}{
%%     ~~Describe \code{L} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x = NULL, L = 11) 
{
    n <- length(x)
    d <- 1:n
    if (n > 2) {
        i <- which(x[-c(1, n)] > x[-(1:2)] & x[-c(1, n)] > x[-c(n - 
            1, n)]) + 1
        ii <- which.max(x[i])
        i <- i[ii]
        t1 <- rep(F, n)
        t1[d < (i - L + 1)] <- T
        t2 <- rep(F, n)
        t2[d > (i + L - 1)] <- T
        ti1 <- declusterDailyQ(x = x[t1], L = L)
        ti2 <- declusterDailyQ(x = x[t2], L = L)
        ti <- c(ti1, i, ti2 + sum(!t2))
    }
    else {
        ti <- numeric(0)
    }
    return(ti)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
