\name{generateBentCableSpiral}
\alias{generateBentCableSpiral}
\title{
generates a generalised bent cable (LQL) logarithmic spiral
}
\description{
generates a generalised bent cable logarithmic spiral
}
\usage{
generateBentCableSpiral(nstart = 200, change = 100, k1 = 35, k2 = 45, g = 0.3, kappa = 2, 
                     dist.zero = 2, noise = 0.01, theta.start = 1e-04, theta.end = 1.25, 
                     equal = FALSE, equal.n = 200, move = TRUE, move.factor = 1000, 
                     plot.it = TRUE, include.title = TRUE, show.change = TRUE)
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
  \item{g}{
related to width of curved transition
}
  \item{kappa}{
power of curve (kappa = 2 is quadratic)
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
number of equally spaced points
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
  \item{include.title}{
text for title (default is blank)
}
  \item{show.change}{
location the change 
}
}
\details{
Generates an outline (x,y) coordinates based on the generalised bent cable of Khan & Kar (2018).

Specifying kappa = 2 as input forces the bent cable to have the quadratic transition of Chiu et al (2006).

The transition location (angle in radian units) is specified as the parameter tau. 
}
\value{
\item{xy}{ data frame with spiral coordinates as x, y, logr, angle, and arc length}
}
\references{
Chiu, G., Lockhart, R. and Routledge, R. (2006), Bent-Cable Regression Theory 
and Applications, Journal of the American Statistical Association, 101, 542-553. 
DOI: 10.1198/016214505000001177 
http://faculty.washington.edu/gchiu/Articles/bentcable-jasa.pdf 

Khan, S.A. and Kar, S.C. (2018). Generalized bent-cable methodology for changepoint
data: a Bayesian approach.  Journal of Applied Statistics, 45(10), 1799-1822 
}
\author{
A.E. Aldridge
}
\note{
g is gamma in the Chiu (2006) and Khan & Kar (2018).  Parameter a2 is estimated by direct calculation.

}
\section{Warning }{ plotting (TRUE) is required for obtaining output data frame} 
\seealso{
\code{generateL2BentCableSpiral}
}
\examples{
\dontrun{
 z <- generateBentCableExtra()
 
}
}
\keyword{ CurvedSpiral } 
