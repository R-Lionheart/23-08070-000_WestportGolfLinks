\name{doLocateChange}
\alias{doLocateChange}

\title{
Interactively locate a sequence of changes in spiral deviations
}
\description{
plots deviations and allows you to interactively locate changes such as maxima and minima
}
\usage{
doLocateChange(spiral.output=spiral.output, nc=NULL, eps.factor=1)
}
\arguments{
  \item{spiral.output}{
output from either spiral fitting or noise cancelling deviations
}
  \item{nc}{
number of changes to locate on the deviation plot
}
  \item{eps.factor}{
multiplier of the average distance between outline points
}
}
\details{
User specified changes on a deviation plot.  The plot is arc length
and vertical deviation.  The locator function is used to interact 
with the plot.  If the number of changes is not given the function 
is terminated. 
By default deviations are plotted with vertical lines for specified 
change locations ( can be extrema, or any feature of the deviations).
}
\value{
\item{locate.change}{sequence of change locations as coordinates (x,y)}
}
\references{
 see minima and maxima in Clark et al (2015) pages 36-38

Clark J., et al (2015) Application of shell spiral deviations methodology to fossil brachiopods. 
Palaeontologica Electronica 18.3.54A:1-39

}
\author{
A.E. Aldridge
}

\examples{
\dontrun{
data(exGiantSquidBeak)
x.df <- exGiantSquidBeak
x <- x.df$x ;  y <- x.df$y
MASS::eqscplot(x,y)
zspiral <- fitAnyLinear( x,y, plot.deviations=TRUE)
zchanges <- doLocateChange( zspiral, nc=2)
zchanges
#
znoise.cancel <- doCancelNoise( x,y,spiral.output=zspiral)
zzchanges <- doLocateChange( znoise.cancel, nc=2)
zzchanges
}
}
\keyword{ Extrema }
