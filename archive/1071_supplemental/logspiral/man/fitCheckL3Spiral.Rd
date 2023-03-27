\name{fitCheckL3Spiral}
\alias{fitCheckL3Spiral}
\title{
locate a SECOND change between THREE linear spirals, given FIRST change location.
}
\description{
Change in location between THREE spirals working either side of the given change for two spirals
}
\usage{
fitCheckL3Spiral(x = x, y = y, m = NULL, axis.start = c(NULL, NULL), 
               start.values = c(a = 2, k1 = 0.2, k2 = 0.2, k3 = 0.2),
               specimen.name = " ", tolerance.step = 0.01, forward = 0, back = 0, 
                plot.ss = TRUE, plot.detail = TRUE, keep.output = FALSE,
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
given change location
}
  \item{axis.start}{
as
}
  \item{start.values}{
initial values for other unknowns in strict order of : distance to start of spiral (a), 
       and two spiral angles (k1, k2) in radian units.
}
  \item{specimen.name}{
name of outline being checked for a graph title
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
Given a change point (m) the function steps along the outline to find if there
is another change location.   At each coordinate step three spirals are fitted.

Steps exclude six points at outline ends and at either side of the given change location.

If plot detail is FALSE then comparisons are made with minimum found for TWO spirals
and the base (or overall minimum) expected for the outline. The defaut (TRUE) shows
the unscaled RSS values at each step along the outline. Gaps indicate nls failure.

Tolerance step is the tolerance input to the nonlinear fitting function nls (base)

This function has not been vectorised, or checked if vectorising could save a lot of 
execution time.  Watching the progress steps gives a sense of how nls is performing with
the starting values. 

If dense coordinates consider decimating the outline before fitting spirals.
}
\value{
location of second change point (so now three spirals)
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
Hint: use a fixed start value for the spiral axis (e.g. the estimate from single spiral fit)
to speed up the testing the coordinate sequence.
}

\section{Warning }{ likely to see gaps in plot where nls has failed to converge} 

\seealso{
\code{fitCheckL2Spiral} provides the given change point for two spirals that is input as \code{ m} in this function.
}
\examples{
\dontrun{
# generate three linear spirals (L3) with angles 35,40,55 and changes at 100,150 (default)
set.seed(1998)
 z <- generateAnyLinear( k1=35,k2=35,k3=40,k4=55, dist.zero=5) 
#
# search first for the first (and largest) change in spiral angle
#
xchk <- fitCheckL2Spiral( x=z$x, y = z$y) # should locate change at 149th coordinate
#
# input this change into searching for a second change in spiral angle
#
 xchk3 <- fitCheckL3Spiral( x=z$x, y=z$y, m=149) # finds second change at 99th coordinate
#
# Verify that this pair of changes (99,149) do give the best possible overall fit
#
 xs3.verify <- verifyL3Change( x=z$x, y=z$y, m=c(99,149) ) 
#
# output the matrix of minima, find the least of these is at (100,150) coordinate locations
# coordinate locations.  Now can fit this final spiral as a L3
  xs3.verify
#
xs3 <- fitAnyLinear( x =z$x, y=z$y, m=c(100,150) ) 
#
xs3[1]  # parameter values or use xs3$parameters1
xs3[2]  # statistics of fit or xs3$parameters2, mssd ratio now close to unity (1.0)
#
}
}
\keyword{ change } 
