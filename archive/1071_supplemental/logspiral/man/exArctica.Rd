\name{exArctica}
\alias{exArctica}
\docType{data}
\title{
 \emph{Arctica islandica} shell outline
}
\description{
Two dimensional outline of the ocean quahog \emph{Arctica islandica} valve
}
\usage{data(exArctica)}
\format{
  A data frame with 1xx observations on the following 2 variables.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
   \item{\code{z}}{a numeric vector}
  }
}
\details{
More than one spiral is best fit. Jones (1983) finds age of 12 years for this
specimen.
}
\source{
Figure 3 in Jones (1983)  
}
\references{
Jones, D. 1983. Sclerochronology: Reading the Record of the Molluscan Shell.
    American Scientist 71(4):384-391 

Jones, D. 1981. Repeating layers in the molluscan shell are not always periodic.
   Journal of Paleontology 55(5) 1076-1082.  See Figure 1.
}
\examples{
\dontrun{ 
# load the data file
data(exArctica)
x <- exArctica$x ;  y <- exArctica$y
#
MASS::eqscplot(x,y)
zzz <- fitAnyLinear(x,y, plot.deviations=TRUE)
}
}
\keyword{datasets}
