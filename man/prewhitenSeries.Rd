\name{prewhitenSeries}
\alias{prewhitenSeries}

\title{
  Prewhiten a time series 
}

\description{
  Assuming an AR1 process superimposed by a linear trend, the time
  series gets prewhitened in an iterative manner until the trend and
  autocorrelation estimates stabilise.
}

\usage{
prewhitenSeries(x, alpha = 0.05, thr = 0.001, max.iter = 100)
}

\arguments{
  \item{x}{numeric vector. Missing values are not allowed and it is
    assumed that the frequency, at which the observation took place, is
    constant.}
  \item{alpha}{real number. Defines the significance level that
    the autocorrelation coefficient at lag 1 needs to pass in order that
    the prewhitening gets applied. To be fully compatible with the
    references below, use \code{1-pnorm(q=0.05,sd=1/sqrt(n))} with
    \code{n} being the sample size.}
  \item{thr}{real number. If the absolute differences of the
    autocorrelation coefficient and slope estimates between two
    iterations fall below \code{thr}, the iteration terminates.}
  \item{max.iter}{integer. The maximum number of iterations.}
}

\details{
  The procedure can be useful in the context of a trend analysis where
  both a trend and an AR1 process might be present in the time series of
  interest. The strategy is to estimate the AR1 process and the trend in
  an iterative manner until the differences between two iterations are
  smaller than \code{thr}.

  Since geophysical processes with negative autocorrelation are rarely
  encountered in nature, the series is tested only for positive
  autocorrelation; the trend is estimated using Sen's slope estimator.

  Monte Carlo experiments show that the present approach to prewhiten a
  time series is especially suitable in case of small sample sizes and
  that it provides a fair compromise between type I and type II
  errors. In particular, the approach tries to balance the AR1 and trend 
  component, leading to a slightly increased risk to detect spurious
  trends in the prewhitend series if the original series consists of an
  AR1 process only; see the references and example below.
}

\value{
  List with entries
  \item{x}{numeric vector. The (eventually prewhitend) time series.}
  \item{prewhitend}{logical. Is \code{x} prewhitend?}
  \item{acoef}{real number. The estimated autocorrelation coefficient at
    lag 1.}
  \item{slope}{real number. The estimated slope of a linear trend used
    to estimate \code{acoef} and hence does not necessarily equal the
    slope of the returned series \code{x}.}
  \item{iter}{integer. The number of iterations.}
}

\references{
  Wang, X. L., and V. R. Swail (2001).
  "Changes of Extreme Wave Heights in Northern Hemisphere Oceans and Related Atmospheric Circulation Regimes".
  Journal of Climate.

  Zhang, X., and F. W. Zwiers (2004).
  "Comment on 'Applicability of prewhitening to eliminate the influence of serial correlation on the Mann-Kendall test' by Sheng Yue and Chun Yuan Wang".
  Water Resources Research.
}

\author{
  Simon S
}

\examples{
\dontrun{
library(trend)

## conduct Monte Carlo experiments
n <- 50
nsim <- 1000
coln <- c('prewhitend','acoef','slope','iter','mk.test')

## check type I error (risk)
## series consists of AR1 process only
m <- matrix(NA,nrow=nsim,ncol=length(coln),dimnames=list(NULL,coln))

for(i in 1:nsim) {
    x <- arima.sim(model=list(ar=0.4),n=n)
    mo <- prewhitenSeries(x=x)
    p.mk <- mk.test(x=na.omit(mo$x),
                    alternative='two.sided',
                    continuity=TRUE)$p.value
    m[i,'prewhitend'] <- mo$prewhitend
    m[i,'acoef'] <- mo$acoef
    m[i,'slope'] <- mo$slope
    m[i,'iter'] <- mo$iter
    m[i,'mk.test'] <- p.mk
}

summary(m)
sum(m[,'prewhitend']==1)/nsim
sum(m[,'mk.test']<0.05)/nsim

par(mfrow=c(1,3))
hist(m[,'acoef'])
hist(m[,'slope'])
hist(m[,'mk.test'])

## check type II error (power)
## series consists of true trend and AR1 process
m <- matrix(NA,nrow=nsim,ncol=length(coln),dimnames=list(NULL,coln))

for(i in 1:nsim) {
    xt <- seq(0,by=0.05,length.out=n)
    xa <- arima.sim(model=list(ar=0.4),n=n)
    x <- xt+xa
    mo <- prewhitenSeries(x=x)
    p.mk <- mk.test(x=na.omit(mo$x),
                    alternative='two.sided',
                    continuity=TRUE)$p.value
    m[i,'prewhitend'] <- mo$prewhitend
    m[i,'acoef'] <- mo$acoef
    m[i,'slope'] <- mo$slope
    m[i,'iter'] <- mo$iter
    m[i,'mk.test'] <- p.mk
}

summary(m)
sum(m[,'prewhitend']==1)/nsim
sum(m[,'mk.test']<0.05)/nsim

par(mfrow=c(1,3))
hist(m[,'acoef'])
hist(m[,'slope'])
hist(m[,'mk.test'])}
}