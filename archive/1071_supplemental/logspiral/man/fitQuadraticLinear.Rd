\name{fitQuadraticLinear}
\alias{fitQuadraticLinear}
\title{
Fits a Quadratic Linear (QL) spiral
}
\description{
Fits  quadratic connected to a linear spiral
}
\usage{
fitQuadraticLinear(x, y, m = NULL, axis.start = c(NULL, NULL), 
                   start.values = c(a1 = 2, k1 = 40, k2 = 55), 
                   angle.deg = TRUE, plot.fit = FALSE, 
                   plot.deviations = FALSE, plot.diagnostics = FALSE, 
                   flip.x = FALSE, move.by = NULL, 
                   tolerance.step = 0.1, trace.check = FALSE,
                   title.label = "")
}
\arguments{
  \item{x}{
x coord
}
  \item{y}{
y coord
}
  \item{m}{
location (mth coordinate) where there is a  change in spirals
}
  \item{axis.start}{
start location of spiral axis as a (x,y) coordinate
}
 \item{start.values}{
initial values for other unknown values in order of
distance axis to to start of linear spiral (a1), two spiral 
angles (k1, k2). The quadratic component is calculated (see details)
}
 \item{angle.deg}{
units for spiral angle (default is degrees)
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
Allows for continual decrease/increase in spiral angle after the change from linear.
The quadratic estimate is calculated from k1, k2, and the change location.
}
\value{
\item{parameters1}{spiral axis location (u,v), and spiral parameters a1, k1, k2, q}  

\item{parameters2 }{ standard deviations of fit (radius) and test of randomness }

\item{xpredict, ypredict}{ are the coordinates for the fitted radius ( translated or flipped outline)}

\item{angle }{is the angle (radians) of the actual & fitted point about the spiral axis }

\item{arc.length}{ is the arc length from the first point (zero) to the last point on an outline}

\item{deviations}{  diference between in radii between actual and fit from the estimated spiral axis}

\item{fitted}{ the fitted radius from estimated spiral axis}

\item{xpred.orig,  ypred.orig }{ coordinates for the fitted radius in the original coordinate axes}

}
\references{
 no references yet
}
\author{
A.E.  Aldridge
}
\note{
Sequentially trial other change locations and check whether the output \code{parameters2} improves, or not.
}
\seealso{
\code{generateQuadraticLinearSpiral}    \code{    fitQuadraticLinear}     \code{    fitBentCable}
}
\examples{
\dontrun{
 z <- generateQuadraticLinearSpiral(k2=0,q= -0.5, equal=F )
xs <- fitQuadraticLinear( x=z$x, y=z$y, m=100, axis.start=c(990,990))
 xs$parameters1
}
}
