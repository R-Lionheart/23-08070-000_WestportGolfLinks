\name{fitQuartic}
\alias{fitQuartic}
\title{
Fits quartic spiral to an outline of x,y coordinates
}
\description{
Fits a QUARTIC (QQ) spiral to given outline coordinates.  Quartic
includes intercept, linear, quadratic, and cubic terms.
}
\usage{
fitQuartic(x, y, axis.start = c(NULL, NULL), flip.x = FALSE, 
                 dist.zero = 2, spiral.angle = 40, 
                 quadratic = 0.02, cubic = 1e-04, quartic = 1e-04, 
                 move.by = NULL, tolerance.step = 0.01, angle.deg = TRUE,
                 plot.fit = FALSE, plot.deviations = FALSE,
                 plot.diagnostics = FALSE, title.label="")
}
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
  \item{spiral.angle}{
initial slope of spiral (in degrees)
}
  \item{quadratic}{
initial quadratic term
}
  \item{cubic}{
initial cubic term
}
  \item{quartic}{
initial quartic term
}
  \item{move.by}{
moves (translates) the entire outline so all coordinates are positive
}
  \item{tolerance.step}{
nls tolerance step
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
Outline coordinate pairs (x,y) are to be in sequential order from the beginning
of growth (umbo in shells).
}
\value{
Outline coordinates (Cartesian and polar) summary statistics, 
deviations from spiral, and fitted values (original coordinate axis
and translated axis).
see fitAnyLinear for values output.
}
\references{
Aldridge, A.E. (1999) Brachiopod outline and episodic growth. 
Paleobiology 25, 471-482.

McGhee, G.R.(1980) Shell form in the biconvex articulate Brachiopoda: 
a geometric analysis. 

Paleobiology 6, 57-76.
}
\author{
A.E.  Aldridge
}
\note{
Useful for flat outlines with most curvature at or near the umbo. Example
is in the data set \code{exRafinesquina} where the adult part of growth matches
a quartic spiral. 

Otherwise, used to exclude this type of spiral to a given outline.
}
\seealso{
\code{generateCurvedSpiral}    \code{      exRafinesquina}
}
\examples{
\dontrun{
# generate a quartic spiral (QQ) that also includes  quadratic and cubic terms 
#
 zqq <- generateCurvedSpiral( quadratic= 0.01, cubic= -0.06, quartic=0.01)
#
# fit this spiral using interactive locating of axis on the graphics window
# use a reasonable location based on the axis from the generated spiral
# or use close to known value for axis and other parameters
#
xqq <- fitQuartic( x= zqq$x, y= zqq$y  )
#
 xqq <- fitQuartic( x= zqq$x, y= zqq$y, axis.start=c(999,999)  )
#
# view the summary statistics
#
xqq[1]  # can also use xc$parameters1
xqq[2]  # can also use xc$parameters2
}
}
