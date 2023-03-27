\name{plotSpiralDeviations}
\alias{plotSpiralDeviations}

\title{
plot deviations from a spiral fit
}
\description{
plots deviations from spiral fit output
}
\usage{
plotSpiralDeviations(spiral.output = spiral.output, 
change = c(NULL, NULL, NULL), 
title.label = "", output.device = 4, units="mm")
}

\arguments{
  \item{spiral.output}{
the fitted spiral  
}
  \item{change}{
location of changes in spiral (if any)
}
  \item{title.label}{
title label, usually a specimen name or code
}
  \item{output.device}{
which device to plot
}
  \item{units}{
units for plot label (default is mm)
}
}
\details{
note the graphic set up of windows
}
\value{
plots without any values returned. Called by \code{fitAnyLinear} and the other fitting functions
}

\author{
A.E. Aldridge
}


\seealso{
\code{fitAnyLinear}
}
\examples{
##none as internal to fitAnyLinear and other fitting functions
\dontshow{
## The function is currently defined as
function (spiral.output = xxx, change = c(NULL, NULL, NULL), 
    xdata = x, ydata = y, title.label = specimen, output.device = 4) 
{
    if (is.null(change)) 
        m <- 0
    else m <- length(change) + 1
    dev.set(output.device)
    deviations <- spiral.output$deviations
    arc.length <- spiral.output$arc.length
    plot(arc.length, deviations, type = "s", xlab = "Secant or Arc length from umbo (mm)", 
        ylab = "Spiral deviations(mm)")
    abline(h = 0, col = 4)
    abline(v = arc.length[change], col = 2, lwd = 2, lty = 3)
    axis(4, labels = F)
    axis(3, labels = F)
    mtext(paste("Spiral deviations for specimen ", title.label), 
        side = 3, outer = T, line = -2, cex = 1.2, col = 4, adj = 0.05)
  }
}
}
\keyword{ deviations } 
\keyword{ display }
