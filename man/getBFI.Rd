\name{getBFI}
\alias{getBFI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
here we need a title in order to check and install the package
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getBFI(y = NULL, win = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{win}{
%%     ~~Describe \code{win} here~~
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
function (y = NULL, win = 5) 
{
    n <- floor(length(y)/win)
    y <- y[1:(n * win)]
    x <- rep(1:n, each = win)
    q <- tapply(y, x, function(x) {
        ifelse(any(is.na(x)), NA, which.min(x))
    }) + seq(from = 0, by = 5, length.out = n)
    i <- 2:(n - 1)
    qq <- matrix(c(y[q[i - 1]], y[q[i]], y[q[i + 1]]), ncol = 3)
    is.qb <- apply(qq, 1, function(x) {
        ifelse(any(is.na(x)), NA, x[2] < x[1] * 0.9 & x[2] < 
            x[3] * 0.9)
    })
    is.qb <- c(NA, is.qb, NA)
    qb <- q[is.qb]
    x <- 1:length(y)
    yy <- approx(x = x[qb], y = y[qb], xout = x)$y
    t.gt <- yy > y
    t.gt[is.na(t.gt)] <- F
    yy[t.gt] <- y[t.gt]
    t.sub <- cbind(qb, c(qb[-1], qb[length(qb)]))
    t.in <- unique(unlist(apply(na.omit(t.sub), 1, function(x) {
        x[1]:x[2]
    })))
    sum(yy[t.in])/sum(y[t.in])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
