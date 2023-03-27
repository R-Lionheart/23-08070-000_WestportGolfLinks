\name{exRafinesquina}
\alias{exRafinesquina}
\docType{data}
\title{
 Rafinesquina sp \emph{outline of a shell outline}
}
\description{
Two dimensional, sagittal section of a strophomenid Ordovician brachiopod, \emph{Rafinesquina}
}
\usage{data("exRafinesquina")}
\format{
  A data frame with 150 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
  }
}
\details{
Example of applying two very different spirals to an outline.
Clark et al (2015): "impossible to fit a spiral to this valve", but
here we have two spirals that fit well with sprial deviations that can be extracted (see Example code
below). 
}
\source{
Figure 8 (left), page 11 of Clark et al (2015).
}
\references{
Clark J., et al (2015) Application of shell spiral deviations methodology to fossil brachiopods. 
Palaeontologica Electronica 18.3.54A:1-39
}
\examples{
\dontrun{
data(exRafinesquina)
x.df <- exRafinesquina
# First part is a L1 spiral
xs1a <- fitAnyLinear( x = x.df$x[1:36], y = x.df$y[1:36], plot.fit=T, plot.deviations=T)
# extract coordinates for second part of the outline
 xx <- x.df$x[-c(1:36)]
 yy <- x.df$y[-c(1:36)]
# fit spiral to second part
xsq <- fitQuartic( xx,yy, plot.fit=T, quadratic=0.2, cubic=0.07,
                   axis.start=c(8, 11.4), plot.deviations=T)
# plot fitted spirals and the digitised outlines
 eqscplot( x.df$x, x.df$y, xlab="", ylab="");axis(4,labels=F);axis(3,labels=F)
 lines(xs1a$xpred.orig, xs1a$ypred.orig, col=2, lwd=2)
 lines(xsq$xpred.orig, xsq$ypred.orig, col=2, lwd=2)
 title( "Rafinesquina sp: outline and fitted spirals", adj=0)
#
# plot concatenated deviations from both spirals
 plot( c(xs1a$arc.length, xsq$arc.length+max(xs1a$arc.length)), 
             c(xs1a$deviations, xsq$deviations), type="s",
        xlab="Arc length (mm)", ylab="Deviation from fitted spirals (mm)")
 abline(v=max(xs1a$arc.length), col=2)
 abline(h=0, col=4)
title( "Rafinesquina sp: spiral deviations", adj=0)
}
}
\keyword{datasets}
