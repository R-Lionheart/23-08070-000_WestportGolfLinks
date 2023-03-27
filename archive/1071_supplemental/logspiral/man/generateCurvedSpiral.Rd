\name{generateCurvedSpiral}
\alias{generateCurvedSpiral}
\title{
generates a curved spiral
}
\description{
generates a continuous  curved (nonlinear) spiral up to the fourth 
power(quartic) of angle about spiral axis.
}
\usage{
generateCurvedSpiral(nstart = 200, dist.zero = 2, k = 50, 
           quadratic = 0.03, cubic = 0.03, quartic = 0.01, 
          noise = 1e-05, theta.start = 1e-04, theta.end = 1.25, 
          equal = FALSE, equal.n = 200, move = TRUE, move.factor = 1000, 
          plot.it = TRUE, print.summary = TRUE, show.change = TRUE)
}
\arguments{
  \item{nstart}{
number of outline points to generate (default 200)
}
  \item{dist.zero}{
initial distance between spiral axis and spiral when angle (theta) is zero 
}
  \item{k}{
spiral angle or first power of angle about axis, units are degrees.
}
  \item{quadratic}{
 second power of angle about axis
}
  \item{cubic}{
third power of angle about axis
}
  \item{quartic}{
fourth power of angle about axis
}
  \item{noise}{
standard deviation of error added to the distance from spiral axis
}
  \item{theta.start}{
angle about axis to first point on outline (a small number)
}
  \item{theta.end}{
angle at end of outline IN RADIANS ( approximate half a circle)
}
  \item{equal}{
should outline coordinates be equally spaced (default is no)
}
  \item{equal.n}{
if equal, then how many coordinates (default same as nstart, 200)
}
  \item{move}{
translate outline so all coordinates positive
}
  \item{move.factor}{
how much to move the outline
}
  \item{plot.it}{
plot the outline (default is yes)
}
  \item{print.summary}{
print summary (default is yes)
}
  \item{show.change}{
show the change on the plot
}
}
\details{
Generates a spiral that is curved when using logarithmic radius, and
angle coordinates.  The input quadratic, cubic and quartic terms can be negative,
positive or zero.  Spiral angle (first power, slope term) is input as degrees, then
converted to radians for calculations.

For a quadratic spiral set to zero the cubic and quartic terms.
}
\value{
\item{xy}{ data frame with spiral coordinates as x, y, logr, angle, and arc length}
}
\references{
 no references yet
}
\author{
A.E. Aldridge
}
\note{
Small changes in curve's input values can create large changes in spiral, and length
of the outline. Creates  a 'flat' looking spiral with axis very near the umbo.

Default is NOT to equally space the generated coordinates. Equal spacing
introduces small errors that can affect confirmation fitting.
}

\section{Warning }{may need to trial and error to find reasonable shape} 

\seealso{
\code{fitQuadratic}    \code{   fitCubic}     \code{  fitQuartic}
}
\examples{
\dontrun{
 
doSetupGraphics()
# Quadratic spiral
 zq <- generateCurvedSpiral( quadratic=0.03, cubic=0, quartic=0)
dev.set(4)
plot( zq$angle, zq$logr) # upwards quadratic curve
#
# Cubic Spiral
 zc <- generateCurvedSpiral( quadratic=0.03, cubic=0.03, quartic=0)
dev.set(4)
plot( zc$angle, zc$logr) # upwards quadratic curve
#
# only the cubic term is specified.
 zc <- generateCurvedSpiral( quadratic=0.0, cubic=0.03, quartic=0)
#
# Quartic spiral
#
 zqq <- generateCurvedSpiral( quadratic= 0.01, cubic= -0.06, quartic=0.01)
dev.set(4)
plot( zqq$angle, zqq$logr) # sinuous log. radius
# flatter spiral
zqqq <- generateCurvedSpiral( quadratic= 0.01, cubic= -0.02, quartic=0.02)
dev.set(5)
plot( zqqq$angle, zqqq$logr) # sinuous log. radius
#
}
}
\keyword{ CurvedSpiral } 
