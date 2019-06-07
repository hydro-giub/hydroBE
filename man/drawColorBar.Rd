\name{drawColorBar}
\alias{drawColorBar}

\title{
  Draw a color bar into the right plot margin
}

\description{
  Image plots usually codify the z values with colors. This function
  helps to draw an appropriate color bar into the right plot margin. 
}

\usage{
drawColorBar(breaks = NULL, col = NULL, usr = par("usr"),
             xleft = 0.05, xright = 0.08, ybottom = 0.25, ytop = 0.25,
             main = "", main.cex = 1.5, main.mar = 0.01,
             lab.cex = 1, lwd = 0.5, seg.len = 0.2)
}

\arguments{
  \item{breaks}{numeric vector. The breaks that are used to bin the
    numerical range. Should be of \code{length(col)+1}.}
  \item{col}{character vector. Defines for each bin a color.}
  \item{usr}{numeric vector. Defines the plotting region in user
    coordinates according to \code{c(x_min,x_max,y_min,y_max)}.}
  \item{xleft}{real number. Sets the left coordinate as a fraction of
    the width defined by \code{usr[1:2]}.}
  \item{xright}{real number. Sets the right coordinate as a fraction of
    the width defined by \code{usr[1:2]}.}
  \item{ybottom}{real number. Sets the bottom coordinate as a fraction of
    the height defined by \code{usr[3:4]}.}
  \item{ytop}{real number. Sets the top coordinate as a fraction of
    the height defined by \code{usr[3:4]}.}
  \item{main}{string. Optional title for the color bar.}
  \item{main.cex}{real number. Character expansion for \code{main},
    which is passed to \code{\link{text}}.}
  \item{main.mar}{real number. Sets the margin of the title relativ to
    the color bar as a fraction of the height defined by
    \code{usr[3:4]}.}
  \item{lab.cex}{real number. Character expansion for the bar labels,
    which is passed to \code{\link{text}}.}
  \item{lwd}{real number. Line width to draw the tick marks and box
    around the color bar.}
  \item{seg.len}{real number. Length of the tick marks as a fraction of
    \code{xright-left}.}
}

\value{
  Returns invisible.
}

\author{
  Simon S.
}

\examples{
breaks <- 1:50
col <- rainbow(49)
z <- matrix(runif(n=100,min=1.1,max=49.9),nrow=10,ncol=10)
par(mar=c(4,5,3,6))
image(z=z,col=col,breaks=breaks)
drawColorBar(breaks=breaks,col=col,main='bar')
}
