\name{convertMetricToPixel}
\alias{convertMetricToPixel}
 
\title{
converts metric coordinates to raster pixel coordinates
}
\description{
metric coordinates are returned to the pixel or raster coordinates
that were used to digitise the outline of an image
}
\usage{
convertMetricToPixel(xeqn = c(1,0,0), yeqn = c(0,1,0), x = x, y = y,  
                     check.input = FALSE, check.output = FALSE)
}
\arguments{
  \item{xeqn}{
the pixel to metric equation for x outline coordinate (three numbers)
}
  \item{yeqn}{
the pixel to metric equation for x outline coordinate (three numbers)
}
  \item{x}{
the x coordinates to be converted
}
  \item{y}{
the y coordinates to be converted.
}
  \item{check.input}{
option to print out the input transformation values for the conversion
}
  \item{check.output}{
option to print the output (inverse matrix) of the transformation values
}
}
\details{
Mostly for a Euclidean transformation : translation and rotation. Affine is also possible.
Default setting is the Identity matrix for no transformation.

The equation numbers are in the order: x coefficient, y coefficient, and intercept. In 2D there is one 
equation for the x coordinate (xeqn) and one for the y coordinate (yeqn).  Input coordinates were
computed through the transformation 2 x 2 matrix (elements a, subscripts i,j) as:

x(metric) = a11*x(pixel) + a12*y(pixel) + xintercept

y(metric) = a21*x(pixel) + a22*y(pixel) + yintercept

The output coordinates use the inverse 2 x 2 matrix (elements b, subscripts i,j)  

x(pixel) = b11*(x(metric) - xintercept) + b12*(y(metric) - yintercept)

y(pixel) = b21*(x(metric) - xintercept) + b22*(y(metric) - yintercept

Equation can be produced by the digitising software used or be calculated using linear regression. 
For example, in Vextractor the key F6 gives the reference points used in the digitising scale 
along with the equations for conversion of pixels to the user units (usually millimetres).

Useful to have the converted coordinates for plotting the fitted spiral, deviations, and 
spiral axis location on top of the original image. 

See the vignette \emph{fitGiantSquid}
}
\value{
coordinates in the scale that was used in the digitised image.


}
\references{
Brannan, D.A., Esplen, M.F., Gray, J.J. 2012. Geometry (2nd ed). Cambridge University Press.

Simon Barthelme (2019). imager: Image Processing Library Based on 'CImg'. R package version 0.41.2.
  https://CRAN.R-project.org/package=imager

Vextractor x64 7.1  http://www.vextrasoft.com/

}
\author{
A.E. Aldridge
}
\note{
assumes matrix inversion is non singular.
}

\seealso{
 \code{eqscplot} in MASS
}
\examples{
\dontrun{
data(exGiantSquidBeak)
xy.df <- exGiantSquidBeak
x <- xy.df$x; y <- xy.df$y  # outline (x,y) coordinates
spiral.fit <- fitAnyLinear(x=x, y=y)  # fitted spiral (L1 in this example)
#
x.axis <- spiral.fit$parameters1[1] # estimated L1 spiral axis location
y.axis <- spiral.fit$parameters1[2] 
x <- spiral.fit$xpred.orig # fitted spiral coordinates in original(metric) values. 
y <- spiral.fit$ypred.orig
# not the correct transformation - used as example 
xeqn <- c(0.0153728, 8.62282*10^-5, 70.0251)
yeqn <- c(-1.1789*10^-5,-0.0153494,45.4761)
#
# convert to pixels the spiral axis and fitted values.
#
xy.axis <- convertMetricToPixel(xeqn=xeqn, yeqn=yeqn, x=x.axis, y=y.axis)
xy.image.df <-  convertMetricToPixel(xeqn=xeqn, yeqn=yeqn, x=x, y=y)
#
# image input as an object and pixels values as an overlay
#
library(imager)  # must be installed first - not part of logspiral requirements (yet)
GiantSquidBeak <- load.image(file="####.JPG") 
plot( GiantSquidBeak, axes = F)
points( xy.image.df$xx, xy.image$yy, cex=0.5, col=2) # overlay fitted points in red
points ( xy.axis[1], xy.axis[2], pch=10,cex=1.5, col=2) # spiral axis location in red
# end of plotting image with overlay of points (can save as pdf, jpg, etc)
rm( GiantSquidBeak ) # remove what can be a rather large object in the workspace.

}
}
