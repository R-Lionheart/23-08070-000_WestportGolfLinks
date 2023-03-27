\name{verifyL3Change}
\alias{verifyL3Change}
\title{
verify the TWO change locations in a three spiral fit
}
\description{
verify the TWO change locations in a three spiral fit
}
\usage{
verifyL3Change(x = x, y = y, m = c(NULL, NULL), pp = c(-2, -1, 0, 1, 2), 
                axis.start = c(NULL, NULL))
}
\arguments{
   \item{x}{
x coord
}
  \item{y}{
y coord
}
  \item{m}{
specified location of TWO change points on an outline
}
  \item{pp}{
sequence of coordinate points either side of specified change points
}
  \item{axis.start}{
start in x, y 
}
}
\details{
Points either side are as above or a vector such as seq(-2,2,1). Location
of minimum for the deviation rss is output. If not the same as for the three
spiral fit, then use best change points for the final three spiral fit.
Output is rss = residual sums of squares from the spiral fit. 

Automatic use of the spiral checking functions may not provide the global
minima for change locations.  

\emph{BEWARE} when really need two criteria - not only minimal RSS, but also
maximum biological signal (e.g. periodicities).
}
\value{
\item{rss.mat }{ matrix of deviation rss's, with rows the first change location}
}
\references{
Aldridge (1999) Brachiopod outline and episodic growth.  Paleobiology 25, 471-482
}
\author{
A.E. Aldridge
}
\note{
The matrix can be input to code for generating a contour plot (eg using lattice package)
}

\seealso{
\code{fitAnyLinear}  	\code{    fitL3Spiral}
}
\examples{
\dontrun{
require(MASS)
doSetupGraphics()
x.df <- generateAnyLinear( k1=30, k2=30, k3=50, k4=40, noise=0.1)
xx <- x.df$x  
yy <- x.df$y
## created three spirals with two change points at 100, 150 points from
## outline start or umbo
#
xspiral <- fitAnyLinear(xx, yy,m=c(99,149), axis.start=c(110, 100))
## fits three spirals varying change locations about 99 and 149 (true values
 # are 100 and 150. If axis.start is not specified, you will be 
 # asked to input a location off graphics device 2.
#
xyz <- verifyL3Change(x = xx, y = yy, m = c(99,149), pp = c(-2, -1, 0, 1, 2)) 
 # xyz is a matrix with rows for first change, and columns for second change
 # the location of minimum is also output, which looking at the matrix should
 # be a 100 and 150.
 #
## also create a contour plot using lattice, but first need vectors
x <- rep( as.numeric( rownames(xyz) ),length(rownames(xyz)) 
y <- rep( as.numeric( colnames(xyz) ),each= length(colnames(xyz) ) )
z <- as.vector(xyz)
#
lattice::contourplot( z ~ x*y, cuts=10,
xlab="First change location",ylab="Second change location")
}
}
\keyword{ change } 
