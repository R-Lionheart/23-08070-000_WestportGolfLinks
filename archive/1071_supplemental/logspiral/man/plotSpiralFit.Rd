\name{plotSpiralFit}
\alias{plotSpiralFit}

\title{
plot results of fitting spiral(s)
}
\description{
plots fitted spiral or spirals with axis location
}
\usage{
plotSpiralFit(spiral.output = spiral.output, 
change = c(NULL, NULL, NULL), xdata = xdata, ydata = ydata, 
title.label = " ", output.device = 5)
}

\arguments{
  \item{spiral.output}{
the fitted spiral  
}
  \item{change}{
location of changes in spiral (if any)
}
  \item{xdata}{
x coords used in fit
}
  \item{ydata}{
y coords used in fit
}
  \item{title.label}{
title label
}
  \item{output.device}{
which device to plot
}
}
\details{
called by fitAnyLinear and some other fits.   Note the graphic set up with four windows
as device numbers 2, 3, 4, and 5.
}
\value{
plots without any values returned
}

\author{
A.E. Aldridge
}


\seealso{
\code{fitAnyLinear}
}
\examples{
##see fitAnyLinear

#
}
\keyword{ display }
