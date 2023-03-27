\name{doCancelNoise}
\alias{doCancelNoise}
\title{
Cancels the noise in deviations from a fitted spiral
}
\description{
Cancels the noise in \bold{vertical or radial deviations} from a fitted spiral after
finding the best smooth value for the outline
}
\usage{
doCancelNoise(x = x, y = y, spiral.output = spiral.output,
              vertical = TRUE, plot.it = TRUE, gtitle = "")
}
\arguments{
  \item{x}{
x coordinate of outline
}
  \item{y}{
y  coordinate of outline
}
  \item{spiral.output}{
spiral.output  the output from a spiral fit
}
  \item{vertical}{
should vertical deviations (if available) be used (default TRUE)
}
  \item{plot.it}{
plot.it   if TRUE then plot the noise cancelled deviations
}
  \item{gtitle}{
gtitle	title for the graphics window (default blank)
}
}
\details{
Applies a smooth to the (x,y) outline using a smooth factor (spar) so that the std. dev. of spiral
deviations will be similar to those from the smooth.  The std. dev. is based on the square of 
successive differences (mssd).
The deviations from smooth are then subtracted from the spiral fit. A check is made if the 
outline was flipped (X axis) for spiral fitting with a positive spiral angle.  If flipped then the vertical deviations from 
smooth are multiplied by -1.  
}
\value{
\item{x}{ original x coordinate}

\item{y}{ original y coordiate of outline}

\item{arc}{ arc length}

\item{spiral.dev}{ vertical deviations from spiral fit }

\item{smooth.dev}{ vertical deviations from the smooth fit}

\item{no.noise}{ noise cancelled spiral deviations}

}
\references{
Bissell, A.F. and Williamson (1998) Successive difference tests - theory and interpretation. 
 Journal of Applied Statistics 15(3) 305-323
}
\author{
A.E. Aldridge
}
\note{
\bold{Assumes outline starts from the umbo.}  You need to manually check that this is so
by plotting the outline and first point.  To change order use the \code{ rev()} function
and check outline before any use of spiral fitting. 

Only linear spiral fitting (L1 to L4) outputs vertical deviations. Otherwise use the function \code{doCompareOutlines}
on actual and fitted outlines.
}

\section{Warning }{If output not looking as being noise cancelled, then maybe sign error in deviations. 
Use vertical = FALSE if any fitting other than linear (L1 to L4) } 

\seealso{
\code{bestSmooth}     \code{     fitAnyLinear }       \code{     arcSmooth2D}
}
\examples{
\dontrun{
require(MASS)
doSetupGraphics()
  xy.df <- generateAnyLinear( k1=30, k2=30, k3=40,k4=40, noise=0.1)
xx <- xy.df$x  
yy <- xy.df$y

## no mouse location and no plotting of results.
xspiral <- fitAnyLinear(xx, yy, axis.start=c(110, 100))
#
## if no axis.start YOU will be prompted to guess axis location
## on graphics device 2 (left mouse click)
 xspiral <- fitAnyLinear(xx, yy)
xspiral.smooth <- doCancelNoise( xx,yy, xspiral, plot.it=T, 
gtitle="Deviations from single spiral fit to two spirals") 
  }
}

\keyword{ smooth } 
\keyword{ deviations } 
