\name{fitQuadratic}
\alias{fitQuadratic}
\title{
Fits a quadratic spiral (Q) to a given outline
}
\description{
Fits a quadratic spiral to an outline.  The quadratic includes terms for 
intercept (distance at zero angle), and slope (spiral angle)
}
\usage{
fitQuadratic(x, y, axis.start = c(NULL, NULL), flip.x = FALSE, 
              dist.zero = 2, spiral.angle = 40, quadratic = 0.02, 
              move.by = NULL, tolerance.step = 0.01, angle.deg = TRUE,
              plot.fit = FALSE, plot.deviations = FALSE, plot.diagnostics = FALSE,
              title.label = "")
}
\arguments{
  \item{x}{
x coordinate of outline
}
  \item{y}{
y coordinate of outline
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
  \item{spiral.angle}{
initial slope of spiral (in degrees) 
}
  \item{quadratic}{
initial value for quadratic term.
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
text for the title of the deviation plot (default none)
}
}
\details{
If spiral angle is in degrees it is converted to radians for computations then
converted back to degrees in the output.
Outline coordinate pairs (x,y) must be in sequential order from the beginning
of growth (umbo in shells).
}
\value{
Outline coordinates (Cartesian and polar) summary statistics, 
deviations from spiral, and fitted values (original coordinate axis
and translated axis).
see fitAnyLinear for examples.
}
\references{
Aldridge, A.E. (1999) Brachiopod outline and episodic growth. 
            Paleobiology 25, 471-482.

McGhee, G.R.(1980) Shell form in the biconvex articulate Brachiopoda: 
a geometric analysis. 
Paleobiology 6, 57-76.
}
\author{
A.E. Aldridge
}
\note{
Useful for almost flat outlines with curvature very near the umbo.
}
\section{Warning }{ read Troubleshooting Guide if not converging or an error} 
\seealso{
\code{generateCurvedSpiral} \code{    fitCubic}   \code{    fitQuartic}   \code{    fitL4Spiral}
}
\examples{
\dontrun{
# generate a quadratic spiral (Q) 
#
zq <- generateCurvedSpiral( quadratic=0.03, cubic=0, quartic=0)
#
# fit this spiral using interactive locating of axis on the graphics window
# use a reasonable location based on the axis from the generated spiral
#
xq <- fitQuadratic( x= zq$x, y= zq$y)
#
# view the summary statistics
#
xq[1]  # can also use xq$parameters1
xq[2]  # can also use xq$parameters2
#
}
}
