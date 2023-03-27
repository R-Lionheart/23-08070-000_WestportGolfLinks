\name{fitBentCable}
\alias{fitBentCable}
\title{
Fits a generalised bent cable spiral to an outline of x,y coordinates
}
\description{
Fits generalised bent cable spiral to given outline coordinates. Three phases
of growth: linear then a curved transition to the final linear phase.
}
\usage{
fitBentCable(x, y, m = NULL, axis.start = c(NULL, NULL), quadratic = TRUE, 
          start.values = c(a = 2, k1 = 36, k2 = 46, g = 0.3, kappa = 2.0), 
         angle.deg = TRUE, plot.fit = FALSE, plot.deviations = FALSE, 
         plot.diagnostics = FALSE, flip.x = FALSE, move.by = NULL, 
         tolerance.step = 0.1, trace.check = FALSE, 
         title.label = "Bent Cable outline")
}
\arguments{
  \item{x}{
x coordinate
}
  \item{y}{
y coordinate
}
  \item{m}{
location coordinate for the transition (not necessarily central)
}
  \item{axis.start}{
start location of the spiral axis as a (x,y) coordinate
}
  \item{quadratic}{
should a quadratic bend be forced (default no quadratic)
}
  \item{start.values}{
initial values for other unknown values in the strict order:
distance axis to to start of spiral (a1), two spiral 
angles (k1, k2), and shape of the transition (g, kappa)
}
  \item{angle.deg}{
units of spiral angle as start values and output (default FALSE)
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
  \item{title.label}{
text for the title of the deviation plot (default none)
}
}
\details{
Generalised bent cable of Khan & Kar (2018) is applied to the outline,
which is converted to polar coordinates based on an initial axis location
(two values input to the axis.start function argument).
  
If quadratic is TRUE then kappa is set at 2.0 for the 
bent cable of Chiu et al (2006). See Khan & Kar (2018) page 1803.

The \code{nls} function is applied to a residual sums of squares to estimate six 
unknown parameters (u, v, a1, k1, k2, g and kappa). The central location of 
transition must be supplied as the mth coordinate, which is the paramter tau
in the generalised bent cable.  Tau is not estimated, but is an output value
as an angle (radian units) along the outline.
}
\value{
\item{parameters1}{spiral axis location (u,v), and spiral parameters a1, k1,a2,k2,g,kappa,tau}  

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
data: a Bayesian approach.  Journal of Applied Statistics, 45(10), 1799-1822 
}
\author{
A.E. Aldridge
}
\note{
g is the gamma in Chiu (2006) and Khan & Kar (2018).  Parameters a2, a3 are estimated by direct calculation.


Bayesian methods using JAGS and STAN cannot be applied because the independent variables (axis locations) are updated at
every iteration.  Other spiral fitting functions can be applied to help find suitable 
starting values.  For example, a \strong{fitL1Spiral} to the beginning of the outline locates spiral axis, initial
intercept, and slope. The functions \strong{fitL2Check}, and a \strong{fitL3Spiral} can help locate starting
values for the transition location,and the slope or spiral angles.
}

\section{Warning }{start or intial (esp. axis) values need to be close to actual so that nls converges} 

\seealso{
\code{ fitLinear2BentCable}. Have quadratic shape to the transition (i.e. kappa = 2) . 
}
\examples{
\dontrun{
 z <- generateBentCableSpiral()
 xs <- fitBentCable(x = z$x, y=z$y, m=100, axis.start=c(1000,1000) ,
        plot.fit=TRUE, plot.deviations=TRUE)
# assumes four graphics windows are open (fit on device 3, and deviations on device 4)
}
}
\keyword{ CurvedSpiral } 
