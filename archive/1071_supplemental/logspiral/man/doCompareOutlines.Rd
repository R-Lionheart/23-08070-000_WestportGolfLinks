\name{doCompareOutlines}
\alias{doCompareOutlines}
\title{
sequental deviations between two outlines
}
\description{
Computes deviations as perpendicular distances
between two outlines with equal or matched points.
}
\usage{
doCompareOutlines(a.df, b.df, plot.deviations = TRUE, print.area = TRUE)
}
\arguments{
  \item{a.df}{
reference data frame with columns named as x and y coordinates
}
  \item{b.df}{
 data frame with columns named as x and y coordinates 
}
  \item{plot.deviations}{
should deviations be plotted (default is TRUE)
}
  \item{print.area}{
should the total area under deviations be printed (default is TRUE)
}
}
\details{
Both outlines should have the same number of coordinates (x,y) and preferably 
equally spaced. 

Area between outlines is the area under deviations when plotted against 
arc length (uses Trapezoid Rule of integration).

Deviations are signed to be positive when beyond the reference outline (a.df). 
The sign also depends on location of the origin so do check that the sign
makes sense, if not multiply deviations by -1.

}
\value{
\item{x1}{x coordinate of data frame a.df}
\item{y1}{y coordinate of data frame a.df}
\item{x2}{x coordinate of data frame b.df}
\item{y2}{y coordinate of data frame b.df}
\item{arc.length}{ arc length from start of outline in a.df}
\item{deviation}{ perpendicular distance from outline a to outline b}
}
\references{
Maxwell E.A. (1958) Elementary Coordinate Geometry (page 81)

Ian L. Dryden (2018). shapes: Statistical Shape Analysis. R package
  version 1.2.4. https://CRAN.R-project.org/package=shapes
}
\author{
A.E. Aldridge
}
\note{
Think of \emph{a.df} has having the raw outline and \emph{b.df} with the fitted or smooth curve 
this outline.  In spiral fitting, deviations are the perpendicular or vertical 
nearest distance from the outline to the fit.  Area is a measure of 'not fitting' 
or error between the fit (or smooth) and the raw outline.

This function is also useful when using ordinary Procrustes to align and match outlines.
See function \code{procOPA} in the \code{shapes} package. Deviations between outlines can 
be plotted with increasing arc length.  

}
\seealso{
\code{ equalSpace}    \code{    arcSmooth2D}   \code{    bestSmooth }
}
\examples{
\dontrun{
require(MASS)
 library(logspiral)
require(MASS)
doSetupGraphics()
# generate a L2 spiral with change at 100th coordinate pair
# and a difference of 3 degrees in spiral angle. Fit a L1 spiral
# then extract fitted coordinates as a data frame.
#
xy.df <- generateAnyLinear( k1=40, k2=40, k3=43,k4=43, noise=0.1)
xys.fit <- fitAnyLinear( x=xy.df$x, y=xy.df$y)
xyfit.coords <- data.frame(x=xys.fit$xpred.orig, y=xys.fit$ypred.orig)
xcompare <-  doCompareOutlines( a.df=x.df, b.df=xyfit.coords)
##
# Fit an L2 spiral and extract fitted coordinates, and compare
#
 xys2.fit <- fitAnyLinear( x=xy.df$x, y=xy.df$y,m=100)
 xyfit2.coords <- data.frame(x=xys2.fit$xpred.orig, y=xys2.fit$ypred.orig)
 xy2compare <-  doCompareOutlines( a.df=xy.df, b.df=xyfit2.coords)
##
# Fit a smooth to the outline and extract fitted coordinates, and compare
#
 xysmooth <- arcSmooth2D( x=xy.df$x, y=xy.df$y)
 xysm.coords <- data.frame( x=xysmooth$xp, y=xysmooth$yp)
 xsm.compare <-  doCompareOutlines( a.df=xy.df, b.df=xysm.coords)
#
## can work well on any pairs of outline shapes
  }
}
\keyword{ smooth }  
\keyword{ deviations}
