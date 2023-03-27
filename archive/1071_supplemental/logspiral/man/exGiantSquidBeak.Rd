\name{exGiantSquidBeak}
\alias{exGiantSquidBeak}
\docType{data}
\title{
Upper beak of a Giant Squid
}
\description{
Upper beak of a Giant Squid (\emph{Architeuthis dux})
}
\usage{data(exGiantSquidBeak)}
\format{
  A data frame with 154 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
  }
}
\details{
Outline is from beak tip (rostrum) to hood.
More than one spiral is best fit.  Deviations indicate an age of 6 years.
}
\source{
Tony Aldridge, from NIWA (Darren Stevens) collection specimen 02/00 labelled
as "v.immature". Aldridge label as  NIWA04 (out of 11 photographed).

NIWA: National Institute of Water and Atmospheric Research (NZ)
}
\references{
Aldridge (2009) Can beak shape help to research the life history of squid?
New Zealand Journal of Marine and Freshwater Research 43  1061-1067
}
\examples{
\dontrun{
data(exGiantSquidBeak)
x.df <- exGiantSquidBeak
x <- x.df$x ;  y <- x.df$y
require(MASS)
eqscplot(x,y)
## fit single spiral.  Note that both four spirals (L4), and 
## the L2BentCable highlight the underlying periodicity.
zzz <- fitAnyLinear( x,y)
}
}
\keyword{datasets}
