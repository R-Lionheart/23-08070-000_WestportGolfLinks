\name{exGryphaea}
\alias{exGryphaea}
\docType{data}
\title{
Lower valve in a Gryphaea
}
\description{
Shell growth line in \emph{Gryphaea arcuata}
}
\usage{data(exGryphaea)}
\format{
  A data frame with 116 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
  }
}
\details{
More than one spiral is best fit.  This is the raw digitised data, yet to be equally spaced.
Common name for this species is Devil's Toenail and is a "mollusc emulating a brachiopod".
}
\source{
Figure 6, page 171.  Lower image. of Jones & Gould (1999)
}
\references{
 Direct Measurement of Age in Fossil Gryphaea: The Solution to a Classic Problem in Heterochrony
Douglas S. Jones and Stephen Jay Gould
Paleobiology
Vol. 25, No. 2 (Spring, 1999), pp. 158-187
}
\examples{
\dontrun{ 
data(exGryphaea)
x <- exGryphaea$x ;  y <- exGryphaea$y
require(MASS)
eqscplot(x,y)
## fit a single spiral and note the deviation shape is
## classic for a two spiral fit.
zzz <- fitAnyLinear( x,y, plot.deviations=T, plot.fit=T)
}
}
\keyword{datasets}
