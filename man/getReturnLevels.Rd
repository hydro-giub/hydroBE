\name{getReturnLevels}
\alias{getReturnLevels}

\title{
  Calculate the return levels from a fitted GEV model
}

\description{
  Calculates the return levels and corresponding confidence intervals for
  a Generalized Extreme Value (GEV) distribution fitted by the
  \bold{\href{https://cran.r-project.org/package=ismev}{ismev}} package.
}

\usage{
getReturnLevels(f, per = c(30,100,300), alpha = 0.05)
}

\arguments{
  \item{f}{object of class \code{gev.fit}. Needs to be the return value
    from the function \code{gev.fit} of the package
    \bold{\href{https://cran.r-project.org/package=ismev}{ismev}}.}
  \item{per}{integer vector. The return periods with respect to the chosen
    block size. For yearly block maxima this amounts to years.}
  \item{alpha}{real number. The confidence level alpha.}
}

\details{
  Return levels and confidence bounds are calculated as described in Coles
  (2001). Confidence bounds follow the delta method. Only stationary GEV
  distributions are supported.
}

\value{
  Numeric matrix with columns 'period' (equals \code{per}), 'level' (the
  return levels), 'lower', and 'upper' (the lower and upper confidence
  bounds with respect to \code{alpha}).
}

\references{
  Coles, S. (2001).
  "An Introduction to Statistical Modeling of Extreme Values".
  Springer Series in Statistics.
}

\author{
  Simon S
}

\seealso{
  package \bold{\href{https://cran.r-project.org/package=ismev}{ismev}}
}

\examples{
\dontrun{
library(ismev)
f <- gev.fit(rnorm(100))
rl <- getReturnLevels(f)
rl}
}
