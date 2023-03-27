\name{fitCheckL3Restricted}
\alias{fitCheckL3Restricted}
\title{
locates a SECOND change between THREE linear spirals, given a FIRST change location.
}
\description{
Locates change between THREE spirals when change testing is restricted between two specified locations, but still fitting the entire outline.
}
\usage{
fitCheckL3Restricted(x, y, m = NULL, axis.start = c(NULL, NULL), specimen.name = " ", 
                     start.values = c(a = 2, k1 = 0.2, k2 = 0.2, k3 = 0.2), 
                     tolerance.step = 0.01, from = NULL, to = NULL, 
                     plot.ss = TRUE, keep.output = FALSE, 
                     no.error.messages = TRUE, orient.check = TRUE)
}
\arguments{
  \item{x}{x coord}
  \item{y}{y coord}
  \item{m}{
the given first change location (as mth coordinate) from start of outline
}
  \item{axis.start}{
start location as (x,y) coordinates
}
  \item{specimen.name}{specimen name used as text for graphic title}
  \item{start.values}{
initial values for other unknowns in strict order of : distance to start of spiral (a), 
 and two spiral angles (k1, k2) in radian units.}
  \item{tolerance.step}{step used in the nls algorithm (see nls)}
  \item{from}{first outline location to begin search for change location}
  \item{to}{ outline location to end search}
  \item{plot.ss}{plot}
  \item{keep.output}{
graphic summary has change points, otherwise can keep a summary of every step
}
  \item{no.error.messages}{keep the silent = T in the try command for nls.}
  \item{orient.check}{check the orientation of the outline (default is to check)}
}
\details{
Given a possible change point (m) the function steps from \bold{specified beginning} (forward 
argument) of the outline to the m th location.  At each coordinate step three spirals are fitted.  

Tolerance step is the tolerance input to the nonlinear fitting function nls (base)

This function has not been vectorised, or checked if vectorising could save a lot of 
execution time.  Watching the progress steps gives a sense of how nls is performing with
the starting values. 

If dense coordinates consider decimating the outline before fitting spirals.

}
\value{
change point
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
\bold{from} is where the fitting starts on the outline, especially useful if there is a pathological shape near
to the umbo (e.g. protoconch, sudden change to incurving) where only a few points (i.e. not enough 
to fit another curve).

\bold{to} is the last outline coordinate to check for change in spiral.  

The purpose of the from/to restriction is to speed up the sequential checking of each coordinate as a possible second spiral change.
}

\section{Warning }{

\bold{from} and \code{to} should both be on one side of the given change.  The function plots these locations
on the outline to assist with any errors generated.

There can be gaps in plot of std. dev. vs outline sequence  \code{plot.ss=TRUE} where nls fails to converge.} 

\seealso{
\code{checkL3Spiral}   \code{    fitCheckL2Spiral}   	\code{    VerifyL3Change  }
}
\examples{
\dontrun{
# generate three linear spirals (L3) with angles 35,55,60 and changes at 100,150 (default)
set.seed(1998)
  z <- generateLinearSpirals( k1=35,k2=35,k3=55,k4=60, dist.zero=5) 
#
# search first for the first (and largest) change in spiral angle.
#
xchk <- fitCheckL2Spiral( x=z$x, y = z$y) # should locate change at 103th coordinate
#
# input this change into searching for a second change in spiral angle
# going FORWARD from this change and note the residual standard deviation

#
xchk3 <- fitCheckL3Restricted( x=z$x,y=z$y, m=103, from=10, to=90)
# find minimal drop in error so no important change in this restricted range.
# next is to try a range AFTER the  given change location
#
xchk3 <- fitCheckL3Restricted( x=z$x,y=z$y, m=103, from=110, to=190)
#
# change estimated at coordinate 153 with smaller residual error  
# 
# Verify that this pair of changes (103,153) do give the best possible overall fit
#
xs3.verify <- verifyL3Change( x=z$x, y=z$y, m=c(103,153),  pp=seq(-4,2,1) )
#
# output the matrix of minima, find the least of these is at (100,150) coordinate locations
# coordinate locations.  Now can fit this final spiral as a L3 spiral
  xs3.verify  # prints out the matrix of coordinate pairs and residual standard deviation
#
xs3 <- fitAnyLinear( x =z$x, y=z$y, m=c(100,150) ) 
#
xs3[1]  # parameter values or use xs3$parameters1
xs3[2]  # statistics of fit or xs3$parameters2, mssd ratio now close to unity (1.0)
#
}
}
\keyword{ change } 
