\name{curvatureNoPenalty}
\alias{curvatureNoPenalty}
\title{
compute outline curvature using spline smoothing
}
\description{
compute curvature along an outline and derivatives
}
\usage{
curvatureNoPenalty(x, y, smooth.factor = 5)
}
\arguments{
  \item{x}{
 x coord of outline (x,y)
}
  \item{y}{
 y coord of outline (x,y)
}
  \item{smooth.factor}{
degrees of freedom, up to the number of unique x values 
}
}
\details{
see \code{  smooth.spline} in base R,  and differential geometry for curvature formula

For a single linear spiral (L1) a plot of 1/curvature vs radius will produce a line
of slope 1/sin(spiral angle).  Or,plot 1/curvature vs arc length from umbo to see the slope.
a change in slope is masked by noise unless there is a >= 5 degree difference in spiral angles 
and low level of outline noise.

Note that smoothing uses base R smooth.spline, which does not smooth the second derivative, 
otherwise use smooth.Pspline with order=4 and method =2,  from the package spline.

The outline coordinates are assumed NOT to be a closed shape.
}
\value{
\item{arc }{ arc length from first coordinate pair}
\item{curvature }{  curvature value}
\item{zdx1}{	first derivative in x direction}
\item{zdx2}{    second derivative in x}
\item{zdy1}{	first  derivative in y direction}
\item{zdy2 }{   second derivature in y}
}
\references{
Chernov N.(2010) Circular and Linear Regression. 
https://people.cas.uab.edu/~mosya/cl/  (accessed 24 Nov, 2019)  Note links to code for fitting.

Gray et al (1997) Modern differential geometry of curves and surfaces with Mathematica (2nd Edition).

Lin et al (2010) Robust and Accurate Curvature Estimation using Adaptive Line Integrals. Journal
on Advances in Signal Processing.  https://doi.org/10.1155/2010/240309

Miller James (2009) Shape curve analysis using curvature.  PhD Thesis Univeristy of Glasglow
http://theses.gla.ac.uk/854/1/2009millerphd.pdf (accessed 23 Sept 2019)
}
\author{
A.E. Aldridge  
}
\note{

Curvature is often unstable at extremes of outline (eg near axis of a spiral). 
When plotting curvature may need to trim values or limit the curvature axis using ylim.

Alternative method is to use circle fitting to arcs along the outline. Curvature is the reciprocal
of the circle radius. Stable circle fitting (Chernov, 2010) can be applied. The curvature at outline 
extremes is better but overall curvature is too noisy to be useful when comparing spirals.
Need for a better algorithm that varies outline points according to curvature. Such an algorithm described in 
Lin et al (2010) and Allward, M. (2019), but is not implemented yet in logspiral.

Penalised splines are NOT used in this function as they risk over-smoothing the outline especially and miss major changes in spirals.
However, smoothing 2nd derivative through spline::smooth.Pspline is the better method. The function is still in progresss.
}
\seealso{
\code{  arcSmooth2D}      
}

\examples{
\dontrun{
require(MASS)
doSetupGraphics() # creates four graphic windows
# generate L1 spiral outline with some noise
#
x.df <- generateAnyLinear( k1=40, k2=40, k3=40,k4=40, noise=0.1)
#
## outline smoothed using spline and arc length using a smooth 
#   value greater than default
#
zc <- curvatureNoPenalty( x.df$x, x.df$y, smooth.factor=15)
#
## spiral parameter values of the generated L1 outline
a <- 2
k <- 1/tan(40*pi/180)
# 
## curvature vs arc length and actual relationship
#
dev.nex()
plot( zc$arc, zc$curvature)
lines (zc$arc,1/( k*zc$arc + a*sqrt(1+k^2)), col=4, lwd=1.5)
#
# alternatively 
dev.next()
plot( zc$arc, 1/zc$curvature, ylim=c(0,200))
lines (zc$arc,k*zc$arc + a*sqrt(1+k^2), col=4, lwd=1.5)
}
#
}

