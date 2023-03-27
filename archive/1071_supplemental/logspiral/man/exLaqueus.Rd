\name{exLaqueus}
\alias{exLaqueus}
\docType{data}
\title{
Laqueus rubellus
}
\description{
Three brachiopod specimens each with dorsal and ventral valve
}
\usage{data(exLaqueus)}
\format{
  A data frame with 1318 observations on the following 4 variables.
  \describe{
    \item{\code{specimen}}{a factor with levels \code{F1} \code{F3} \code{FR}}
    \item{\code{valve}}{a factor with levels \code{dorsal} \code{ventral}}
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
  }
}
\details{
 See References for which are juvenile and adult brachiopods. Easier to apply spiral fitting if you first
extract a specimen and a valve from the data frame.
}
\source{
Professor Alberto Perez-Huerta (University of Alabama)
}
\references{
Perez-Huerta A., et al (2014) Brachiopod shell spiral deviations (SSD): implications for
trace element proxies. Chemical Geology 374-375, 13-24.
}
\examples{
\dontrun{
data(exLaqueus)
x.df <- exLaqueus
 table(x.df$specimen, x.df$valve)
x1.df <- x.df[x.df$specimen=="F1", ]
x <- x1.df$x[x1.df$valve=="ventral"]
y <- x1.df$y[x1.df$valve=="ventral"]
require(MASS)
eqscplot(x,y)
eqscplot( x1.df$x, x1.df$y,pch=16, cex=0.7)
xs <- fitAnyLinear( x=x, y=y, plot.deviations=T)
}

}
\keyword{datasets}
