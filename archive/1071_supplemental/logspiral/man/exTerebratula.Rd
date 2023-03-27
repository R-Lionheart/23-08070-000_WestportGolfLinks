\name{exTerebratula}
\alias{exTerebratula}
\docType{data}
\title{  Terebratula \emph{fossil section}

}
\description{
dorsal and ventral valves as digitised, without equal spacing
}
\usage{data(exTerebratula)}
\format{
  A data frame with 709 observations on the following 3 variables.
  \describe{
    \item{\code{valve}}{a character vector}
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
  }
}
\details{
A particulary difficult outline because anterior is flattened. Best to remove part of the outline before fitting spirals.
}
\source{
 Professor Alberto Perez-Huerta (University of Alabama)
}
\references{
Clark J., et al (2015) Application of shell spiral deviations methodology to fossil brachiopods. Palaeontologica Electronica 18.3.54A:1-39
}
\examples{
\dontrun{
data(exTerebratula)
z.df <- exTerebratula
x <- z.df$x[z.df$valve=="ventral"]
y <- z.df$y[z.df$valve=="ventral"]
#
dev.new()
MASS::eqscplot(x,y)
dev.new()
MASS::eqscplot( z.df$x, z.df$y)
}
}
\keyword{datasets}
