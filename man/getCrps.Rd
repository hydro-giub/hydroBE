\name{getCrps}
\alias{getCrps}

\title{
  Calculate the continuous ranked probability score 
}
\description{
  The continuous ranked probability score (CRPS) is intended to verify
  a probabilistic prediction, i.e. a prediction in the form of a probability
  distribution. Informally, it judges how close this distribution
  envelops the verifying observation. 
}

\usage{
getCrps(cdfx, cdfy, obs)
}

\arguments{
  \item{cdfx}{numeric vector. The quantiles belonging to the
    probabilities defined in \code{cdfy}.}
  \item{cdfy}{numeric vector. The probabilities belonging to the
    quantiles defined in \code{cdfx}.}
  \item{obs}{real number. The value of the observation corresponding to
    the probabilistic prediction.}
}

\details{
  \code{cdfx} can have a variable step width.
}

\value{
  Real number, i.e. the CRPS value.
}

\references{
  Hersbach, H. (2000).
  "Decomposition of the continuous ranked probability score for ensemble prediction systems".
  Weather and Forecasting.
}

\author{
  Simon S
}

\note{
  \itemize{
    \item The step width in \code{cdfx} should be small enough such that
    a linear approximation between the points is reasonable.
    \item Step functions (i.e. deterministic predictions with the
    entire probability mass at a single value) should be avoided since
    the numerical integration fails (see the 'Examples' below).
  }
}

\examples{
## normal distribution
obs <- 0
cdfx <- seq(from=-5,to=5,by=0.01)
cdfy <- pnorm(q=cdfx)
getCrps(cdfx=cdfx,cdfy=cdfy,obs=obs)

## 'deterministic' prediction:
## in theory we should end up with the mean absolute error
## however, the numerical integration of the step function fails
obs <- 0
cdfx <- c(-3,-1,-1,2)
cdfy <- c(0,0,1,1)
getCrps(cdfx=cdfx,cdfy=cdfy,obs=obs)
}
