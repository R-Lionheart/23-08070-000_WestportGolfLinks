\name{PeakCycle}
\alias{PeakCycle}
\title{
finds maxima (minima) in a sequence of points
}
\description{
finds maxima in a timeseries
}
\usage{
PeakCycle(Data = Data, SearchFrac = 0.02)
}
\arguments{
  \item{Data}{
single vector of data values
}
  \item{SearchFrac}{
how much to searh around the suspected maximum
}
}
\details{
see examples
}
\value{
list of maxima
}
\references{
  Detecting cycle maxima (peaks) in noisy time series (In R) - Stack Overflow 27/06/2013 09:24
  Link to Scholkmann paper on "automatic multiscale-based peak detection"
  http://www.mdpi.com/1999-4893/5/4/588
  "I've tried the "turningpoints" function in the "pastecs" package but it seemed to be too sensitive
  (i.e.,detected too many peaks)".   by R. Batt
}
\author{
William Constantine and Donald Percival for R package \code{wmtsa}
}
\note{
depends on shape of peak whether it can be identified.
Uses a fixed mother wavelet = Mexican hat from the 
double derivative of Gaussian shape.  
}
\seealso{
\code{doLocateChange} function that allows you to manually locate change on a 
graphics device.  

Other ways of finding max and min using  complete wavelet, 
empirical mode, or synchro-squeezed decompositions.  
}
\examples{
\dontrun{
doSetupGraphics()
# create an outline with two spirals, change at the 100th coordinate
xy.df <- generateAnyLinear( k1=35, k2=35, k3=45, k4=45)
  # fit this outline with one spiral, manually guessing axis location
  # at the prompt on graphics device 2.
xys <- fitAnyLinear( xy.df$x, xy.df$y, plot.deviations=T)
  # find the peaks or maxima in following command
  # can reverse sign of the deviations to find minima
#
 max <- PeakCycle( Data=xys$deviations, SearchFrac=0.1)
 min <- PeakCycle( Data=-1*xys$deviations, SearchFrac=0.1)
## check by plotting
#
plot(xys$arc.length, xys$deviations,type="s")
 points(xys$arc.length[max[,1]], xys$deviations[max[,1]], pch=16,cex=1,col=4)
 points(xys$arc.length[min[,1]], xys$deviations[min[,1]], pch=15,cex=1,col=2)


}
}
\keyword{ Extrema }

