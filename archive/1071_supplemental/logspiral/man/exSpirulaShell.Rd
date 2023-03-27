\name{exSpirulaShell}
\alias{exSpirulaShell}
\docType{data}
\title{
\emph{Rams horn coiled shell:}  Spirula spirula
}
\description{
Shell of \emph{Spirula spirula}
}
\usage{data(exSpirulaShell)}
\format{
  A data frame with 962 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
  }
}
\details{
An example of fitting \bold{several whorls} of accretionary growth.

Same specimen as for the \emph{S. spirula} beak data. 
More than one spiral is best fit.  Chambers show up in the deviation
plot, especially if \code{doCancelNoise} is used on the spiral fit. 
}
\source{
Image from Dr. Mark Norman (Dec. 2003)  Museum Victoria, Melbourne, Australia
}
\references{
http://tolweb.org/Spirula_spirula/19989  
(accessed 20 Nov. 2019)
}
\examples{
\dontrun{ 
data(exSpirulaShell)
x <- exSpirulaShell$x ;  y <- exSpirulaShell$y
MASS::eqscplot(x,y)
## fit a single spiral WITHOUT the first ten points
zzz <- fitAnyLinear( x[-c(1:10)],y[-c(1:10)], 
		plot.deviations=T, plot.fit=T)
xnonoise <- NoiseCancelDeviations(spiral.output=zzz)
}
}
\keyword{datasets}
