\name{fitCheckL2Spiral}
\alias{fitCheckL2Spiral}
\title{
locates where two linear spirals (L2) change on an outline
}
\description{
Locates best outline candidate for the change between TWO connected, linear spirals
}
\usage{
fitCheckL2Spiral(x , y , axis.start = c(NULL, NULL), 
         start.values = c(a = 2, k1 = 0.2, k2 = 0.2), 
         specimen.name = " ", tolerance.step = 0.1, 
         forward = 0, back = 0, plot.ss = TRUE, 
         plot.detail = FALSE, keep.output = FALSE, 
         no.error.messages = TRUE, orient.check = TRUE)
}
\arguments{
  \item{x}{
x coord
}
  \item{y}{
y coord
}
  \item{axis.start}{
start in x, y 
}
  \item{specimen.name}{
specimen name
}
  \item{start.values}{
initial values for other unknowns in strict order of : distance to start of spiral (a),
 and two spiral angles (k1, k2) in radian units.
}
  \item{tolerance.step}{
for nls convergence
}
  \item{forward}{
start this many points from umbo (default 8)
}
  \item{back}{
only go as far from end of outline ( default 8)
}
  \item{plot.ss}{
plot the residual deviation ( default is yes)
}
  \item{plot.detail}{
plot all relevant rss's (the default), otherwise only the usual rss
}
  \item{keep.output}{default is no as usually only need minimum location}
  \item{no.error.messages}{default is yes to suppress what can be many non convergences}
 \item{orient.check}{
check the orientation of the outline (default is to check)
}
}
\details{
Iteration number and change point on outline can differ because some iterations do not converge.

This function has not been vectorised, or checked if vectorising could save a lot of 
execution time.  Watching the progress steps gives a sense of how nls is performing with
the starting values. 
}
\value{
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
By default the iteration counts are flushed to the console (in tens).  Warning 
messages from \code{nls} have been suppressed (\code{silent = TRUE} in the \code{try} function). 
Can reduce the \code{ nls} tolerance and narrow the outline range to check stability of change location.

Using \code{optim} (instead of \code{nls}) increases success in the fitting sequence, but it is slower.
Use of sapply barely reduces the time to check the coordinate sequence for change in spiral. 

DO VERIFY THE CHANGE point by varying the number of outline points (see forward and back arguments)


}

\section{Warning }{ expect some gaps if convergence impossible along outline} 

\seealso{
fitAnyLinear
}
\examples{
\dontrun{

doSetupGraphics()
set.seed(1998)
# generate two linear spirals (35, and 40 degrees) with default change at 100 coordinate
 z <- generateAnyLinear( k1=35,k2=35,k3=40,k4=40, dist.zero=5)  
#
# now locate where spirals change with interactive guess at axis location

 xchk <- fitCheckL2Spiral( x=z$x, y = z$y)

# can locate a starting axis location based on fitting a single linear spiral (not shown)
#
 xchk <- fitCheckL2Spiral( x=z$x, y = z$y, axis.start=c(130,115)) 

# fit a two linear spiral fit based on the change found at 100th coordinate

xs2 <- fitAnyLinear( x=z$x, y=z$y, m=100, plot.deviations=T, plot.fit=T)
xs2[2] # for statistics on the fit
#
# By default, the fit is plotted on device 3, and deviations on device 4
# You can also check any other coordinate in sequence from umbo by changing m
# and viewing the increase (decrease) in the residual standard deviation
#
xs2a <- fitAnyLinear( x=z$x, y=z$y, m=101, plot.deviations=T, plot.fit=T)
xs2a[2] # for statistics on the fit
xs2b <- fitAnyLinear( x=z$x, y=z$y, m= 99, plot.deviations=T, plot.fit=T)
xs2b[2] # for statistics on the fit
#
}
}
\keyword{ change } 
