\name{exNautilisShell}
\alias{exNautilisShell}
\docType{data}
\title{
Nautilis shell section
}
\description{
Shell section of a \emph{Nautilis}
}
\usage{data(exNautilisShell)}
\format{
  A data frame with 262 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
  }
}
\details{
An example of fitting \strong{several whorls of accretionary growth}.

More than one spiral is best fit.  Not equally spaced coordinates and
you may need to double using the function  \code{equalSpace}
}
\source{
Image from Dr. Mark Norman (~2003)  Museum Victoria, Melbourne, Australia
}
\references{
Mark Norman: Museum Victoria, Melbourne, Australia 
}
\examples{
\dontrun{ 
data(exNautilisShell)
x <- exNautilisShell$x ;  y <- exNautilisShell$y
#
MASS::eqscplot(x,y)
zzz <- fitAnyLinear( x,y,plot.deviations=T, plot.fit=T)
}
}
\keyword{datasets}
