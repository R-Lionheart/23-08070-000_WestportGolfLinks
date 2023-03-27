\name{fitLinear2BentCable}
\alias{fitLinear2BentCable}
\title{
Fits a linear-to-bent cable spiral on an outline of x,y coordinates
}
\description{
Fits linear connected to a generalised bent cable spiral on the given outline coordinates. So there are FOUR phases
of growth: two linear then a quadratic transition to a final linear phase (ie three linear
phases and one curved phase of growth).
}
\usage{
fitLinear2BentCable(x, y, m = c(NULL, NULL), axis.start = c(NULL, NULL), quadratic = TRUE, 
          start.values = c(a0 = 2, k0 = 36, k1 = 40, k2 = 55, g = 0.3, kappa = 2.0), 
          angle.deg = TRUE, flip.x = FALSE, move.by = NULL, tolerance.step = 0.1, 
          trace.check = FALSE, plot.fit = FALSE, plot.deviations = FALSE, 
          plot.diagnostics = FALSE, title.label = "fit to Linear to Bent Cable outline ")
}
\arguments{
  \item{x}{
x coordinate
}
  \item{y}{
y coordinate
}
  \item{m}{
location coordinates for change between linear phases and centre of transition
}
  \item{axis.start}{
starting location of spiral axis in (x,y) coordinates
}
  \item{quadratic}{
should a quadratic bend be forced (default no quadratic)
}
  \item{start.values}{
initial values for other unknown values in order of
distance axis to to start of first linear spiral (a0), three spiral 
angles (k0, k1, k2), and the shape of the transition (g, kappa)
}
  \item{angle.deg}{
units of spiral angle as start values and output (default FALSE)
}
  \item{flip.x}{
do we flip the outline to ensure positive spiral angle values
}
  \item{move.by}{
do we move (translate) the outline to ensure all coordinates are postive
}
  \item{tolerance.step}{
tolerance step for nls used in the fitting (minscale not specified)
}
  \item{trace.check}{
do we wish to trace the steps of the nls algorithm (default FALSE)
}
  \item{plot.fit}{
plot the outline with fitted values (default FALSE)
}
  \item{plot.deviations}{
plot the radial deviations from spiral fit (default FALSE)
}
  \item{plot.diagnostics}{
plot diagnostics for deviations from fitted spiral (see Detail below)
}
  \item{title.label}{
text for the title of the deviation plot (default none)
}
}
\details{
Initial linear growth abruptly changes to generalised bent cable of Khan & Kar (2018). 
The fit applies to the outline converted to polar coordinates based on an initial axis 
location (two values input to the axis.start function argument).  

The \code{nls} function is applied to residual sums of squares to estimate seven unknown 
parameters (u, v, a0, k0, k1, k2, g and kappa).  If quadratic is TRUE then kappa is set
 at 2.0 for the bent cable of Chiu et al (2006). See Khan & Kar (2018) page 1803. See Khan & Kar (2018) page 1803.

The change of the first linear is first of two locations in the input for m.  The central location
 of transition must be supplied as the 2nd coordinate, which is the paramter tau
in the generalised bent cable.  Tau is not estimated, but is an output value
as an angle (radian units) along the outline. Other spiral parameters (a1, a2) are estimated by 
calculation (i.e. not through nls).
}
\value{
\item{parameters1}{spiral axis location (u,v), and spiral parameters a0, k0, a1, k1, a2, k2, g, kappa, tau}  

\item{parameters2 }{ standard deviations of fit (radius) and test of randomness }

\item{parameters3 }{ N of outline points, degrees of freedom, and was outline flipped}

\item{parameters4 }{ start and end of transition: angle, coord no. and arc distance }

\item{xpredict, ypredict}{ are the coordinates for the fitted radius ( translated or flipped outline)}

\item{angle }{is the angle (radians) of the actual & fitted point about the spiral axis }

\item{arc.length}{ is the arc length from the first point (zero) to the last point on an outline}

\item{deviations}{  diference between in radii between actual and fit from the estimated spiral axis}

\item{fitted}{ the fitted radius from estimated spiral axis}

\item{xpred.orig,  ypred.orig }{ coordinates for the fitted radius in the original coordinate axes}

}
\references{
Chiu, G., Lockhart, R. and Routledge, R. (2006), Bent-Cable Regression Theory 
and Applications, Journal of the American Statistical Association, 101, 542-553. 
DOI: 10.1198/016214505000001177 
http://faculty.washington.edu/gchiu/Articles/bentcable-jasa.pdf 


Khan, S.A. and Kar, S.C. (2018). Generalized bent-cable methodology for changepoint
data: a Bayesian approach.  Journal of Applied Statistics, 45(10), 1799-1822. 
}
\author{
A.E. Aldridge
}
\note{
g is gamma of Chiu (2006)
}

\section{Warning }{start values need to be close to final so that nls converges} 

\seealso{
 \code{fitBentCable} and using kappa for more general shape to the transition. 
}
\examples{
\dontrun{
 z <- generateLinear2BentCableSpiral()
 xs <- fitLinear2BentCable(x = z$x, y=z$y, m=c(30,150), axis.start=c(1000,1000) ,
  plot.fit=TRUE, plot.deviations=TRUE)
# assumes four graphics windows are open (fit on device 3, and deviations on device 4)

}
}
\keyword{ CurvedSpiral } 
