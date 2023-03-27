\name{plotDiagnostics}
\alias{plotDiagnostics}
\title{
Four diagnostic plots from a spiral fit
}
\description{
Produces four plots in one graphic window:

Spiral deviations against distance from umbo

Log distance from spiral axis against angle about axis

Normal probability plot of deviations

Cumulative periodogram of deviations in sequence from umbo
}
\usage{
plotDiagnostics(spiral.output = spiral.output, change = c(NULL, NULL, NULL), 
                output.device = 5)
}
\arguments{
  \item{spiral.output}{
output from the spiral fitting (a list)
}
  \item{change}{
locations (coordinate sequence from umbo) of change points
}
  \item{output.device}{
graphic window for diagnostic output (default is device 5)
}
}
\details{
May need to create graphic windows by using \code{dev.new()}


\code{cpgram} is from MASS package, using Fourier frequencies with a default light taper.

A warning is given if there are extreme deviations ( probability quantile = 0.95)
at or near (within 4 coordinate pairs) of a change location. 
}
\value{
 2 x 2 graphics layout on the one window.
}
\references{
Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with
  S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0
}
\author{
A.E. Aldridge
}
\note{
see documentation in MASS for cpgram: checks for randomness of deviations,
percentage of total deviation variance can be allocated to various frequencies
}


\seealso{
\code{fitAnyLinear}    	\code{   plotSpiralFit}  \code{    plotSpiralDeviations}
}	
\examples{
\dontrun{
z <- generateAnyLinear()
xs <- fitAnyLinear( x = z$x, y=z$y, plot.diagnostics=T)
}
}
\keyword{ display }
