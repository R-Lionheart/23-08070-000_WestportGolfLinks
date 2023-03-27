\name{fitCubic}
\alias{fitCubic}
\title{
Fits a cubic spiral to an outline of x,y coordinates 
}
\description{
Fits a cubic spiral to given outline coordinates.  Cubic
includes constant (intercept), linear and quadratic terms.
}
\usage{
fitCubic(x, y, axis.start = c(NULL, NULL), flip.x = FALSE, 
               dist.zero = 2, angle.start = 40, 
               quadratic.start = 0.02, cubic.start = 0.001, 
               move.by = NULL, tolerance.step = 0.01, angle.deg = TRUE,
               plot.fit =FALSE, plot.deviations = FALSE, 
               plot.diagnostics = FALSE, title.label="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x coordinate
}
  \item{y}{
y coordinate
}
  \item{axis.start}{
location (u,v) coordinates as a starting axis for spiral being fitted.
}
  \item{flip.x}{
flip the x axis (default, no flipping) so outline is anticlockwise.
}
  \item{dist.zero}{
initial distance to outline start when angle is zero
}
  \item{angle.start}{
initial slope of spiral (in degrees) 
}
  \item{quadratic.start}{
initial quadratic term
}
  \item{cubic.start}{
initial cubic term for fitting
}
  \item{move.by}{
moves (translates) the entire outline so all coordinates are positive
}
  \item{tolerance.step}{
nls tolerance 
}
  \item{angle.deg}{
units for spiral angle (default is degrees)
}
  \item{plot.fit}{
plot the outline with fitted values (default FALSE)
}
  \item{plot.deviations}{
plot the radial deviations from spiral fit (default FALSE)
}
  \item{plot.diagnostics}{
plot diagnostics for deviations from fitted spiral (see Detail below)
}
  \item{title.label}{
text for title (default none)
}
}
\details{
If spiral angle is in degrees it is converted to radians for computations then
converted back to degrees in the output.
Outline coordinate pairs (x,y) are to be in sequential order from the beginning
of growth (umbo in shells).

Parameters are estimated using nls.  Intitial or starting values are function arguements,
including the axis location. If initial axis is not not specified then you are asked to
interactively locate the start of the spiral axis.
}
\value{
Outline coordinates (Cartesian and polar) summary statistics, 
deviations from spiral, and fitted values (original coordinate axis
and translated axis).
see fitAnyLinear for examples of output and meaning.
}
\references{
Aldridge, A.E. (1999) Brachiopod outline and episodic growth. 
Paleobiology 25, 471-482.

McGhee, G.R.(1980) Shell form in the biconvex articulate Brachiopoda: 
a geometric analysis. Paleobiology 6, 57-76.
}
\author{
A.E. Aldridge
}
\note{
Useful for outlines that appear mostly 'flat' with most curvature very near the umbo.
}
\section{Warning }{ read Troubleshooting Guide if not converging or an error} 

\seealso{
\code{generateCurvedSpiral}    \code{    fitQuadratic}    \code{    fitQuartic}
}
\examples{
\dontrun{
# generate a cubic spiral that also includes a quadratic term 
#
zc <- generateCurvedSpiral( quadratic= 0.06, cubic= -0.03, quartic=0)
#
# fit this spiral using interactive locating of axis on the graphics window
# use a reasonable location based on the axis from the generated spiral
#
 xc <- fitCubic( x= zc$x, y= zc$y)
#
# view the summary statistics
#
xc[1]  # can also use xc$parameters1
xc[2]  # can also use xc$parameters2
#
}
}
