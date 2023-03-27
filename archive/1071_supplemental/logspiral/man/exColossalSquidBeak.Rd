\name{exColossalSquidBeak}
\alias{exColossalSquidBeak}
\docType{data}
\title{
Upper beak of a Colossal Squid
}
\description{
Upper beak rostrum of a Colossal Squid (\emph{Mesonychoteuthis hamiltoni})
}
\usage{data("exColossalSquidBeak")}
\format{
  A data frame with 392 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
  }
}
\details{
Outline is from beak (rostrum) to hood, digitised to 42 pixels per mm.
A transition or bent cable spiral is best fit.  Deviations indicate an age of at least 7 years.
}
\source{
AE Aldridge, from Te Papa collection specimen M.316689, more information in
 Remeslo et al (2019), page 122.
}
\references{

Aldridge (2009) Can beak shape help to research the life history of squid?
New Zealand Journal of Marine and Freshwater Research 43  1061-1067


Remeslo et al (2019). Distribution and biology of the colossal squid, \emph{Mesonychoteuthis
hamiltoni}: New data from depredation in toothfish fisheries and sperm whale stomach 
contents.

Deep-Sea Research Part I 147, 121-127.
}
\examples{
\dontrun{
 library(logspiral) 
 library(MASS)  # install if not already available
 library(wmtsa) # install if not already available
doSetupGraphics() # applies only to the Windows platform
                  # otherwise type dev.new() four times
#
xy.df <- exColossalSquidBeak # copy of the 392 outline coordinates
#
csbeak.df <- equalSpace( x=xy.df$x, y=xy.df$y, nequal=600) 
#
xx <- csbeak.df$x; yy <- csbeak.df$y # the beak outline coordinates
#
set.seed(6543) # to reproduce the results in Aldridge (20??)
#
L1features <- computeL1DeviationFeatures(x=xx, y=yy, plot.it=FALSE)
#
# you need to click on graph to locate an initial spiral axis. 
# Note that the start of the outline has a red solid symbol.
# After computing the L1 features the outline will have a 
# blue cross for the initial axis and a red, closed cross for the estimated L1 axis
# Note: loads wavelet package wmtsa (install if not already available)

csbeakSpirals <- compareFeaturesToCatalogue(spiral.features= L1features,
                  plot.detail = FALSE,output.results=TRUE) 

# L1 spiral fitted as the object csbeak.L1
#
csbeak.L1 <- fitAnyLinear( x=xx,y=yy, 
             axis.start=c(141,21), plot.deviations=TRUE )
#
## CAN SKIP THE CHECKING OF CHANGES and proceed
## direct to fitting the remaining spirals below
####################################################
# First change(L2 spiral)
checkL2 <- fitCheckL2Spiral( x=xx, y=yy, 
                            axis.start=c(142, 21))
#
# progresses through outline (may take some time)
# finds change point at coordinate 443 with min error 0.111084
#
# Second change (L3 spiral)
# see if there is a second change location along outline
# takes some time, especially slow 1/3 through outline
#
checkL3 <- fitCheckL3Spiral( x=xx, y=yy, m=443, 
                            axis.start=c(142,21) )
# 
# finds second change at coordinate 87 with min error 0.1003496
#
# Third change (L4 spiral)  is there a third possible change
#
checkL4 <- fitCheckL4Spiral( x=xx, y=yy, m=c(87,443), 
                            axis.start=c(142,21) )
#
# finds a third minima at 371, but the fitting fails
# in part of the outline (approx 150 to 250 coordinates)
# minimum error now 0.09393627
########################################################
## FITIING ALL SPIRALS suggested from catalogue ###
#
csbeak.L2 <- fitAnyLinear( x=xx,y=yy,m=c(443), 
         axis.start=c(141,21),plot.deviations=TRUE )
csbeak.L3 <- fitAnyLinear( x=xx,y=yy,m=c(87, 443), 
             axis.start=c(141,21),plot.deviations=TRUE )
csbeak.L4 <- fitAnyLinear( x=xx,y=yy,m=c(87,371, 443), 
             axis.start=c(141,21),plot.deviations=TRUE )
# 
# Transition spirals, LQL and L2QL and their nonquadratic 
# options as objects gLQL and gL2QL
#
csbeak.LQL <- fitBentCable( x=xx, y=yy, m=443,
            axis.start=c(141,21),trace.check=F,
    start.values=c(a=10, k1=29, k2=25, g=0.05, kappa=2), 
            quadratic=T, plot.deviations=T)
#
csbeak.gLQL <- fitBentCable( x=xx, y=yy, m=443,
             axis.start=c(144,11),trace.check=F,
    start.values=c(a=10, k1=29, k2=25, g=0.05, kappa=2), 
            quadratic=F, plot.deviations=T,
             title.label="Generalised Bent Cable")
#
csbeak.L2QL <- fitLinear2BentCable( x=xx, y=yy, m=c(87,443),
               axis.start=c(144,11),trace.check=F,
     start.values=c(a0=10, k0=32, k1=29, k2=25, g=0.05, kappa=2), 
               quadratic=T, plot.deviations=T)
#
csbeak.gL2QL <- fitLinear2BentCable( x=xx, y=yy, m=c(87,443),
                axis.start=c(144,11),trace.check=F,
     start.values=c(a0=10, k0=32, k1=29, k2=25, g=0.05, kappa=2), 
                quadratic=F, plot.deviations=T,
               title.label="Generalised Linear to Bent Cable")
# 
# Choice of best fitting spiral is the L2QL fit
#
# Now a Wavelet decomposition of L2QL spiral deviations
#
csbeak.wavelets <- computeDeviationWavelets( spiral.output=csbeak.L2QL,
                   wave.levels=9 )
# 
# plots the wavelet with maximum variance (level 5 on power of 2)
# can plot each wavelet of the decomposition, e.g. level 6
#
plot( csbeak.L2QL$arc.length, csbeak.L2QL$deviations, type="s")
lines( csbeak.L2QL$arc.length, csbeak.wavelets$V6,col="blue",lwd=2)
}
}
\keyword{datasets}
