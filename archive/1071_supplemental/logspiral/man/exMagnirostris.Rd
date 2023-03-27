\name{exMagnirostris}
\alias{exMagnirostris}
\docType{data}
\title{
Galapagos finch beak : Geospiza magnirostris
}
\description{
Bird upper and lower beak of one specimen.
}
\usage{data(exMagnirostris)}

\format{
  A data frame with 75 observations on the following 3 variables.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
    \item{\code{side}}{a character vector}
  }
}
\details{
 Only the upper, dorsal surface of the beak has enough curvature and points
suitable for a spiral fit. Source image was from a paper reprint so there was not
the resolution for detail digitising. All specimens in Figure 5 of 
Grant (2001) were amenable to spiral fitting. Grant (2001), Grant & Grant (2008)
use beak length, width and thickness (not spiral angles) to distinguish finch species. }
\source{
Figure 5 of Grant(2001) 
}
\references{
Grant, P. (2001) Reconstructing the evolution of birds on islands : 100 years
 of research. Oikos 92: 385-403.

Grant, P. & Grant, R. (2008) How and why species multiply. Princeton. 218p.
}
\examples{
\dontrun{
doSetupGraphics()
z.df <- exMagnirostris
## extract the dorsal or upper beak.
z.df <- z.df[ z.df$side=="dorsal", ]
x <- z.df$x
y <- z.df$y
MASS::eqscplot(x,y)
## fit one spiral.  Note that the outline is flipped for fitting.

gm.spiral <- fitAnyLinear( x, y, plot.deviations=TRUE, plot.fit=T)
}

}
\keyword{datasets}
