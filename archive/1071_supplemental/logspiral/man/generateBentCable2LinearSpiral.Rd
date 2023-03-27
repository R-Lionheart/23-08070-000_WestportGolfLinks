\name{generateBentCable2LinearSpiral}
\alias{generateBentCable2LinearSpiral}
\title{
generates a linear to generalised bent cable spiral
}
\description{
generates a L2QL (LLQL)curved spiral with the LQL a generalised bent cable
}
\usage{
generateBentCable2LinearSpiral(nstart = 200, change = c(50, 150), 
     k1 = 35, k2 = 40, k3 = 55, g = 0.3, kappa = 2, dist.zero = 2, 
     noise = 1e-04, theta.start = 1e-04, theta.end = 1.25, 
     equal = FALSE, equal.n = 200, move = TRUE, move.factor = 1000, 
     plot.it = TRUE, include.title = TRUE, 
     show.change = TRUE, print.summary = TRUE)
}
\arguments{
 \item{nstart}{
number of outline points to generate (default 200)
}
  \item{change}{
where the first is the centre of the quadratic transition, and the second locates the change
from bent cable to the final linear part with slope k3.
}
  \item{k1}{
spiral angle, or slope of first linear part, BEFORE the quadratic transition
}
  \item{k2}{
spiral angle or slope of second linear part, AFTER the quadratic transition
}
  \item{k3}{
spiral angle, or slope of last linear part AFTER bent cable (in degrees)
}
  \item{g}{
width of the transition about change location  
}
  \item{kappa}{
curved shape of transition ( kappa = 2 is quadratic)
}
  \item{dist.zero}{
distance to outline when angle is zero.
}
  \item{noise}{
standard deviation of error added to the distance from spiral axis
}
  \item{theta.start}{
angle about axis to first point on outline (a small number)
}
  \item{theta.end}{
angle at end of outline IN RADIANS ( approximate half a circle)
}
  \item{equal}{
should outline coordinates be equally spaced (default is no)
}
  \item{equal.n}{
if equal, then how many coordinates (default same as nstart, 200)
}
  \item{move}{
translate outline so all coordinates positive
}
  \item{move.factor}{
how much to move the outline
}
  \item{plot.it}{
plot the outline (default is yes)
}
  \item{include.title}{
text for title for plot(default is blank)
}
  \item{show.change}{
show the change location on the plot
}
  \item{print.summary}{
print summary (default is yes)
}
}
\details{
Generates a spiral that is curved and straight when using logarithmic 
radius, and angle coordinates.  

Input are three slopes: first to second then the third (all in degrees that 
are converted to radians). The transition between first and second slopes is a 
curve, leading to the generalised bent cable terminolgy.
Useful to plot logr and angle (see Value below) to judge the change in slopes as
being reasonable for the spirals being studied.
}
\value{
\item{xy}{ data frame with spiral coordinates as x, y, logr, angle, and arc length }
}
\references{

Aldridge (1999) Brachiopod outline and episodic growth. Paleobiology 25, 471-482

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
A shape that is often detected through the shape of deviations in a two spiral (L2) 
fit of an outline.  
}
\section{Warning }{may need to trial and error to match a specific outline}
\seealso{
\code{fitAnyLinear} using up to four spiral angles to match the quadratic transition.

\code{generateBentCable    generateAnyLinear    fitLinear2BentCable  fitBentCable2Linear}     
}
\examples{
\dontrun{
require(MASS)
doSetupGraphics()
z <- generateLinear2BentCable() 
# all the default settings are applied
xs <- fitLinear2BentCable( x=z$x, y=z$y, m=c(30,150), 
                          axis.start=c(1000,1000)  )
}
}
\keyword{ CurvedSpiral } 