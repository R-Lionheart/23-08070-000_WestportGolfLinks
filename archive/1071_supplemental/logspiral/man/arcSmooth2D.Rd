\name{arcSmooth2D}
\alias{arcSmooth2D}
\title{ smooth (spline) an outline and output deviations
}
\description{
Smooth an outline using arc length and splines
}
\usage{
arcSmooth2D(x = x, y = y, smooth.factor = NULL,summary=FALSE,
 plot.deviation=FALSE, plot.it = FALSE, print.summary= FALSE, 
output = TRUE)
}

\arguments{
  \item{x}{
x coord
}
  \item{y}{
y coord
}
  \item{smooth.factor}{
spar value between zero and 1
}
  \item{summary}{
print a summary of the smooth fit
}
  \item{plot.deviation}{no plotting of deviation as a default}
  \item{plot.it}{
plot
}
 \item{print.summary}{
print a summary of the smoothing
}
  \item{output}{
output yes or no
}
}
\details{
See Base R \bold{\code{smooth.spline}} documentation for the range of spar (smooth parameter) values. A NULL value for smooth.factor
forces an estimate of a smoothing factor (see \strong{bestSmooth}).
}
\value{
\item{arc}{arc length (starting at zero)}
\item{deviation}{  signed perpendicular distance from smooth to the outline coordinates }
\item{x, y}{ coordinates of the smoothed outline }
}
\references{
Miller J. (2009) Shape curve analysis using curvature. PhD Thesis Univerisity of Glasglow

http://theses.gla.ac.uk/854/1/2009millerphd.pdf  (accessed 24 Sept. 2019)

Maxwell E.A. (1958) Elementary Coordinate Geometry page 81
}
\author{
A.E. Aldridge 
}
\note{
Each coordinate (x, y) is separately smoothed against arc length for fitted x, y as
a smooth. Too much smoothing loses detail in spiral deviations. 

A smooth factor near to 1.0 has large outline deviations that merges useful detail (e.g. periodicities) with the background noise. On the other hand, a factor close to zero tracks the outline and has little variation in deviations. The best smooth (see the function \strong{bestSmooth}) aims to remove background noise and leave only the useful deviation detail.
}

\examples{
\dontrun{
# create four graphic windows 
doSetupGraphics()
# generate a two spiral outline with noise
x.df <- generateAnyLinear( k1=30, k2=30, k3=40, k4=40, noise=0.1)
xx <- x.df$x  
yy <- x.df$y
## outline is smoothed using spline and arc length using default smooth value

zsmooth <- arcSmooth2D(x=xx, y= yy, plot.it=TRUE)

## determine a reasonable spar value (see details)
bestspar <- bestSmooth( xx, yy, plot.it=TRUE)
# can also print bestspar to view the value
bestspar  

## now smooth the outline using this value
zsmooth <- arcSmooth2D( xx,yy, smooth.factor=bestspar, plot.it=TRUE)

## automatically choose best spar and produce smooth and deviations
zsmooth <- arcSmooth2D( xx,yy, smooth.factor=NULL, plot.it=FALSE)
plot( zsmooth$arc, zsmooth$zdeviation, type="l")

}
}
\keyword{ smooth }
