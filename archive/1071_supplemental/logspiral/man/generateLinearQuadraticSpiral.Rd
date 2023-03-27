\name{generateLinearQuadraticSpiral}
\alias{generateLinearQuadraticSpiral}
\title{
generates a linear connected to quadratic spiral  (LQ)
}
\description{
generates a connected linear then quadratic (LQ) spiral with a given change location
}
\usage{
generateLinearQuadraticSpiral(nstart = 200, change = 100, k1 = 40, k2 = 50, q = 0, 
                 dist.zero = 2, noise = 0.01, theta.start = 0.001, theta.end = 1.25,
                 equal = FALSE, equal.n = 200,
                 move = TRUE, move.factor = 1000, plot.it = TRUE, show.change = TRUE)
}

\arguments{
  \item{nstart}{
number of outline points to generate
}
  \item{change}{
location of change (tau)
}
  \item{k1}{
first expansion angle
}
  \item{k2}{
second expansion angle
}
  \item{q}{
quadratic expansion value
}
  \item{dist.zero}{
distance between axis and first outline coordinate (angle = 0)
}
  \item{noise}{
noise level ( standard deviation in coordinates)
}
  \item{theta.start}{
initial angle at start of outline ( a small number)
}
  \item{theta.end}{
final angle at end of outline 
}
  \item{equal}{
create equally spaced outline coordinates
}
  \item{equal.n}{
number o equally spaced points
}
  \item{move}{
move the outline
}
  \item{move.factor}{
how much to move or translate the outline
}
  \item{plot.it}{
plot the outline or not
}
  \item{show.change}{
location the change 
}
}
\details{
Method is not arc length based so NOT equally spaced by default. If checking a fit
using  \code{fitLinearQuadratic} do not use equally spaced values from this function.
}
\value{
 data frame with spiral coordinates as x, y, logr, angle, and arc length
}
\references{
 no reference yet
}
\author{
A.E. Aldridge
}
\note{
There is redundancy in specified parameter values
}
\seealso{
quadratic then linear spirals : \code{  generateQuadraticLinearSpiral} 
}
\examples{
\dontrun{
z <- generateLinearQuadraticSpiral(k2=0,q= -0.5, equal=F )
xs <- fitLinearQuadratic( x=z$x, y=z$y, m=100,axis.start=c(990,990))
#
z <- generateLinearQuadraticSpiral( dist.zero=5,k1=40,k2=55, equal=F  ) 
xsq <- fitLinearQuadratic( x=z$x, y=z$y, m=50, axis.start=c(990,990))
xsq$parameters1

}
}
\keyword{ CurvedSpiral } 
