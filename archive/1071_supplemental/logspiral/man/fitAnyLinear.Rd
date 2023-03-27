\name{fitAnyLinear}
\alias{fitAnyLinear}
\title{
fit up to four connected or piecewise logarithmic spirals.
}
\description{
fit up to four connected, piecewise linear spirals: L1, L2, L3 and L4.
}
\usage{
fitAnyLinear(x, y, m = c(NULL, NULL, NULL), axis.start = c(NULL, NULL), 
  specimen = "", res1 = NULL, res2 = NULL, tolerance.step = 0.01, flip = FALSE,
  plot.fit = FALSE, plot.deviations = FALSE, plot.diagnostics = FALSE, 
  print.input = TRUE, orient.check=TRUE)
}
\arguments{
  \item{x}{
x coord
}
  \item{y}{
y coord
}
  \item{m}{
change points for spirals (max three)
}
  \item{axis.start}{
axis start coordinates x, y
}
  \item{specimen}{
specimen name
}
  \item{res1}{
lower limit for deviation plot
}
  \item{res2}{
upper limit for deviation plot
}
  \item{tolerance.step}{
tolerance step to nls
}
  \item{flip}{
flip the outline if needed
}
  \item{plot.fit}{
plot fitted spiral and outline (default is none)
}
  \item{plot.deviations}{
plot spiral deviations (default is none)
}
  \item{plot.diagnostics}{
plot diagnostic plot (default is none)
}
  \item{print.input}{
plot the input to the function, especially change points
}
 \item{orient.check}{
check the orientation of the outline (default is to check)
}
}
\details{
Assumes outline is in a anticlockwise direction.   If orient.check is TRUE (default),
then test for outline direction using first ten points.  If clockwise then the
outline is flipped about the Y (vertical) axis for computations. Values of fitted 
outline are flipped back to original orientation of outline (see Values).

The first outline point is assigned an angle of zero plus a tiny angle (eps =0.00001,
see function \code{cumulativeAngle} ). 

Spiral parameter k is in radian units.  Convert to degrees by \code{  (180/pi)*atan(1/k) }

Parameters are estimated using \code{nls} with the default Gauss-Newton algorithm. The number of change points (m) determines which spiral is
fitted. Intitial or starting values are embedded inside the nls call for linear spirals. Initial 
axis location is in the function argument axis.start, if not specified then you are asked to
interactively locate the start of the spiral axis.

Convergence works well when starting spiral angles are large (approximately 80 degrees, or 
0.2 as radian on the logarithmic scale). If not converging, check the Troubleshooting Guide,
or copy & run a fitting function changing nls starting values (useful to have trace set to TRUE).

A spiral deviation is the radial difference between actual and fit from the estimated
spiral axis. Vertical deviation is perpendicular distance from actual point to fitted spiral.

}
\value{
\item{parameters1}{spiral axis location (u,v), and spiral parameters a, k}  

\item{parameters2 }{ standard deviations of fit (radius) and test of randomness }

\item{parameters3 }{ N of outline points, degrees of freedom, and was outline flipped (1=yes)}

\item{xpredict, ypredict}{ are the coordinates for the fitted radius ( translated or flipped outline)}

\item{angle }{is the angle (radians) of the actual & fitted point about the spiral axis }

\item{arc.length}{ is the arc length from the first point (zero) to the last point on an outline}

\item{deviations}{  diference between in radii between actual and fit from the estimated spiral axis}

\item{v.deviations}{ the perpendicular distance from estimated spiral to outline coordinate}

\item{fitted}{ the fitted radius from estimated spiral axis}

\item{xpred.orig,  ypred.orig }{ coordinates for the fitted radius in the original coordinate axes}


}
\references{
Aldridge (1999) Brachiopod outline and episodic growth.  Paleobiology 25, 471-482

Bissell & Williamson (1998) Successive difference tests - theory and interpretation
Journal of Applied Statistics 15(3) 305-323
}
\author{
A.E. Aldridge
}
\note{
Assumes outline starts from the umbo.  You need to manually check that this is so
by plotting the outline and first point.  To change order use the \code{rev()} function then
and check outline before any use of spiral fitting.

Test of randomness is the mean square successive difference (mssd) test.  Unity is random,
greater than 1 is evidence of sawtooth, and less than one for less frequent oscillations.
}

\section{Warning }{ may not always converge so read Troubleshooting Guide for workarounds} 

\seealso{

\code{fitL1Spiral}    \code{     fitL2Spiral}     \code{     fitL3Spiral}  \code{     fitL4Spiral}

\code{fitCheckL2Spiral }    \code{     fitCheckL3Spiral}

\code{fitCheckL3Restricted }    \code{     fitCheckL4Spiral}
}
\examples{
\dontrun{

doSetupGraphics()
set.seed(1998) # set the random number generator so results can be reproduced.
#
# generate single linear logarithmic spiral (L1) using all defaults, including noise
#
 z <- generateLinearSpirals() 
#
# fit this L1 spiral, specifying the initial guess of axis location  
#      
 xs <- fitAnyLinear(x = z$x, y = z$y, axis.start=c(160,112), plot.deviations=T, plot.fit=T)
#
# print to the console the spiral parameters and summary the fit from the fitted object
xs[1]  # for the parameters, you can also use xs$parameters1
xs[2]  # for the measures about the fit itself, you can also use xs$parameters2
# 
# fit a single linear spiral, where initial axis location is interactive in a graphics window
## you will be prompted to guess axis location on graphics device 2 (right mouse click)
# a red symbol is the first point of the outline - this should be at the spiral start, NOT the end
#
 xs <- fitAnyLinear(x = z$x, y = z$y)
# 
## See Vignette on Linear Spirals for two, three and four connected spirals

}
}

