\name{importAwa}
\alias{importAwa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import time series from Awa
}
\description{
Imports time series from files provided by the Amt fuer Wasser und Abfall - Kanton Bern (Awa). Tries to check the variables consistency and to merge files if feasible.
}
\usage{
importAwa(dir = NULL, files = NULL, quiet = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{string. Import all files in directory \code{dir}.}
  \item{files}{character vector. Import all files specified in \code{files}.}
  \item{quiet}{logical. Print summary?}
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
d <- '/home/Downloads/'
\dontrun{df <- importAwa(dir=d)}   

f <- c('/the/first/Bafu/file1.asc','/the/second/Bafu/file2.asc')
\dontrun{df <- importAwa(files=f)} 
}