\name{getReturnLevelsGev}
\alias{getReturnLevelsGev}

\title{
GEV return levels
}
\description{
Returns the return level and corresponding confidence intervals of
fitted GEV model.
}
\usage{
getReturnLevelsGev(f = NULL, rp = NULL, a = NULL)
}

\arguments{
\item{f}{object as returned by the function \code{gev.fit} from package
  ismev}
\item{rp}{integer vector; return periods with respect to the chosen
  block size}
\item{a}{numeric; confidence level alpha}
}

\details{
Return levels and confidence bounds are calculated as described in Coles
2001. Confidence bounds follow the delta method.
}

\value{
matrix
}

\references{
Coles, S. (2001). "An Introduction to Statistical Modeling of Extreme Values".
Springer Series in Statistics.
}

\author{
Simon S
}

\seealso{
Package \bold{\href{https://cran.r-project.org/package=ismev}{ismev}}
}

\examples{
# do do
}
