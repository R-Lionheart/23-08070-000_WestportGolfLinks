\name{fitCheckL4Spiral}
\alias{fitCheckL4Spiral}
\title{
Locate THIRD change between FOUR linear spirals, given TWO change locations.
}
\description{
Checks the possibility of a fourth spiral given two change points
}
\usage{
fitCheckL4Spiral(x, y, m = c(NULL, NULL), axis.start = c(NULL, NULL), 
      start.values = c(a = 2, k1 = 0.2, k2 = 0.2, k3 = 0.2, k4 = 0.2), 
      specimen.name = " ", tolerance.step = 0.01, forward = 0, back = 0, 
      plot.detail = TRUE, plot.ss = TRUE, keep.output = FALSE, 
      no.error.messages = TRUE, orient.check = TRUE)
}

\arguments{
  \item{x}{
x coord
}
  \item{y}{
y coord
}
  \item{m}{
the given two change locations (as mth coordinates) from start of outline
}
  \item{axis.start}{
start location as (x,y) coordinate
}
  \item{specimen.name}{
specimen name used as text for graphic title
}
  \item{start.values}{
initial values for other unknowns in strict order of : distance to start of spiral (a), 
 and three spiral angles (k1, k2, k3) in radian units.
}
  \item{tolerance.step}{
step used in the nls algorithm (see nls)
}

  \item{forward}{forward, start to the fitting}
  \item{back}{
back from end of selected outline
}
  \item{plot.ss}{
plot the residual sums of squares along the outline
}
  \item{plot.detail}{
plot the sequential deviation rss (don't include other rss)
}
  \item{keep.output}{
graphic summary has change points, otherwise can keep a summary of every step
}
  \item{no.error.messages}{
keep the silent = T in the try command for nls.
}
 \item{orient.check}{
check the orientation of the outline (default is to check)
}
}
\details{
Given two change points (m) the function steps along the outline to find if there
is another change location.   At each coordinate step four spirals are fitted.

If plot detail is FALSE then comparisons are made with minimum found for TWO spirals
and the base (or overall minimum) expected for the outline.

Fitting steps around the given change points, 6 locations either side of each.

If forward and back are used to eliminate part of the outline, the fitting does NOT
include these eliminated parts of the outline. 

Tolerance step is the tolerance input to the nonlinear fitting function nls (base)

This function has not been vectorised, or checked if vectorising could save a lot of 
execution time.  Watching the progress steps gives a sense of how nls is performing with
the starting values. 

If dense coordinates consider decimating the outline before fitting spirals.

Error is output when there are no values for m, or the m values are NOT in a strict
increasing sequence.
}
\value{
location of third change point (so now four linear spirals)
\item{yseq}{ iteration number }
\item{ystep}{coordinate pair along outline where a change }
\item{ss.usual}{ residual deviation at the change point }
\item{axis.x}{ horizontal location for axis of best two spiral  }
\item{axis.y}{ vertical location for axis of best two spiral  }
}
\references{
Aldridge (1999) Brachiopod outline and episodic growth.  Paleobiology 25, 471-482
}
\author{
A.E. Aldridge
}
\note{
It can take time to step through all possibilities with a lot of warnings.
Consider using a fixed start value for the spiral axis (e.g. the estimate from single spiral fit)
to speed up the testing the coordinate sequence.
}

\section{Warning }{ likely to see gaps in plot where nls has failed to converge} 

\seealso{
\code{fitCheckL2Spiral}

\code{checkL3Spiral}  \code{    fitCheckL3SpiralRestricted}   \code{    verifyL3Change} 
}
\examples{
\dontrun{
set.seed(1998)
# four spirals with angles 35, 40, 45, 55 with changes at 50, 100, 150 (default)
#
 z <- generateLinearSpirals( k1=35,k2=40,k3=45,k4=55, dist.zero=5) #
#
# checking for two and three spiral changes end up with changes at 101, 150 (actual 100,150)
# 
# verify now if there is a third change (ie four linear spirals) before, inbetween,
# or after these two changes.  Not use of axis starting location to reduce iterations (and time)
# 
xchk4 <- fitCheckL4Spiral( x=z$x, y = z$y, m=c(101,150) ,axis.start=c(135,112))
#
# fit the final four spiral and note that the mssd ratio is still way small
# inspection of deviations shows a step change at the second change. 
xs4 <- fitAnyLinear(x=z$x, y=z$y, m=c(50,101,150), axis.start=c(135, 112) )
#
xs4[2] # statistics of fit
#
# manually checking changes either side shows that changes at 50,100,150 
# not only have a little less residual sd, but more importantly the mssd ratio is better
#
xs4 <- fitAnyLinear(x=z$x, y=z$y, m=c(50,100,150), axis.start=c(135, 112) )
#
xs4[2] # statistics of fit
#
}
}
\keyword{ change } 
