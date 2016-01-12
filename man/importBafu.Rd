\name{importBafu}
\alias{importBafu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import time series from Bundesamt fuer Umwelt (Bafu).
}
\description{
to do
}
\usage{
importBafu(dir = NULL, files = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{string. Import all files in directory \code{dir}.}
  \item{files}{character vector. Import all files specified in \code{files}.
}
}
\details{
Either \code{dir} or \code{files} can be specified, but not both. \code{dir} takes precedence.
}
\value{
data frame
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dir = NULL, files = NULL) 
{
    scan("/home/sschick/Downloads/RhoBri_Q_2346_Monatsmittel.asc", 
        sep = "\n", what = list(NULL), blank.lines.skip = F)
  }
}
