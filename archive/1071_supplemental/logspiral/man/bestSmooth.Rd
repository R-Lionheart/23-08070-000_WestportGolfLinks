\name{bestSmooth}
\alias{bestSmooth}
\title{
finds the best smoothing value to use with arcSmooth2D
}
\description{
Find the best spline by stepping
through values of smoothing value, spar.  Criterion is maximum 
curvature of increase in standard deviation about smooth
Alternatively, the best spar is matched to a a user specified target noise value.
}
\usage{
bestSmooth(x, y, start = 0.01, end = 0.98, delta = 0.005, target = NULL, plot.it = FALSE)
}

\arguments{
  \item{x}{
x coordinate
}
  \item{y}{
y coordinate
}
  \item{start}{
beginning spar value (default 0.005) 
}
  \item{end}{
end spar value ( default 0.98)
}
  \item{delta}{
step increase in spar value (default 0.005)
}
  \item{target}{
the noise target to match with spar 
}
  \item{plot.it}{
plots of standard deviation and curvature with increasing spar values (default no plot)
}
}
\details{
spar is the value for smoothing parameter see the help for \strong{smooth.spline} in Base R. The best smoothing value is defined when the rate of change in standard devation (curvature) is a maximum with increasing spar values. 
Useful to set \code{  plot.it=TRUE} to view that curvature does indeed have a global maximum.  Plots are placed on graphic windows 2 and 3.  
}
\value{
  \item{best}{ the spar value at max curvature of increasing std deviation}
}
\references{
see \code{help(smooth.spline)} in base R
}
\author{
A.E.  Aldridge
}
\note{
See details on \code{smooth.spline} where spar set by cross validation.  Too much smoothing
and we lose important detail on the actual outline deviations. Too little overfits.

If a target noise is supplied the spar is found to match this noise in the 2D outline 
vertical (perpendicular) deviations from smooth (see \strong{arcSmooth2D} ).
}

\seealso{
\code{doSetupGraphics}      \code{    arcSmooth2D}   \code{     doCancelNoise }
}
\examples{
\dontrun{
# create four graphic windows
doSetupGraphics()
# generate a two spiral (L2) outline with noise
x.df <- generateAnyLinear( k1=30, k2=30, k3=40, k4=40,noise=0.1)
#
## outline smoothed using spline and arc length using default smooth value
zsmooth <- arcSmooth2D(x=x.df$x, y=x.df$y, plot.it=TRUE)
#
## determine a reasonable spar value (see details)
xspar <- bestSmooth( xx, yy, plot.it=TRUE)
# can print xspar to view the value
## outline smoothed using spline and arc length using best smooth value
zsmooth <- arcSmooth2D(xx, yy, smooth.factor=xspar, plot.it=TRUE)
zsmooth <- arcSmooth2D(xx, yy, smooth.factor=0.78, plot.it=TRUE)
}

}

\keyword{ smooth }
