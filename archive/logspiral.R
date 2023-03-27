
library(logspiral)
library(devtools)


install.packages("remotes")
remotes::install_github("wconstan/ifultools")
devtools::install_github("wconstan/splus2R")
library("splus2R")
devtools::install_github("wconstan/wmtsa")
library("wmtsa")


doSetupGraphics()

data(exLaqueus)
x.df <- exLaqueus

# extract the specimen F1 and its ventral valve coordinates

x1.df <- x.df[x.df$specimen=="F1", ]
x <- x1.df$x[x1.df$valve=="ventral"]
y <- x1.df$y[x1.df$valve=="ventral"]

# flip the valve so accretion tracks anticlockwise, and
# is in the positive quadrant (avoids negative spiral parameters)

x <- -1*x + 100

## fit an L1 spiral and plot the spiral deviations
# because a starting location is not provided for the spiral axis
# you will be asked to supply one by clicking on a graphics window
# Choose inside the valve, nearer to the umbo or posterior of the valve

xs1 <- fitAnyLinear(x=x,y=y, plot.deviations=T)

# compute Wavelet and Fourier deviation features

z <- computeL1DeviationFeatures(x= x, y=y, plot.it=T)

# compare these features to the catalogue, but no printing
# or saving of results (see help for more detail on function)

compareFeaturesToCatalogue(spiral.features=z,plot.detail=TRUE, output.results=F)

#

## fit an L2 spiral, but first find the location of spiral change

xcheckL2 <- fitCheckL2Spiral(x=x, y=y, plot.detail=F)


# should highlight at or near the 251st coordinate as
# the spiral change on the outline


xL2 <- fitAnyLinear(x=x,y=y, m=251, axis.start=c(55,12), plot.deviations=T)

# print to the console the estimate of the standard deviation of
# spiral deviations.

xL2$parameters2[1]

#
## the L3 spiral fit, first find the location of the second spiral change
#

xcheckL3 <- fitCheckL3Spiral(x=x, y=y,m=251, plot.detail=F)

#
# should highlight at or near the 70th coordinate as second change point
# and an estimate of the spiral axis location.
#

xL3 <- fitAnyLinear(x=x,y=y, m=c(70,251),axis.start=c(60,12), plot.deviations=T)

#
## the linear to bent cable spiral fit
#

xL2QL <- fitLinear2BentCable(x=x, y=y, m=c(70,251), axis.start=c(57,12),
                             start.values=c(a0=3.7, k0=48, k1=45, k2=55, g=0.3, kappa=2),
                             plot.deviations = T, plot.fit = T)

#
# type help(logspiral) or ?logspiral to view all functions
# and further explanations of the package.
#

# Cubic (C) spiral fit, subsequently rejected as an unstable fit
# (note that this use requires an initial axis as screen input)
#

xCubic <- fitCubic(x=x,y=y, quadratic.start=0.005,cubic.start=-0.01, plot.deviations=T, plot.fit=T)

#
# Quartic (QQ) spiral fit also rejected, but this time because of more
# variability in the spiral deviations (compared to the L2, L3 fits)
#

xQuartic <- fitQuartic(x=x,y=y, quadratic=0.005, cubic=0.01,
                       
                       quartic=-0.005,plot.deviations=T)

#
# L2 is the final choice of spiral for this outline
## Fourier spectrum of the L2 spiral deviations


doSpectrum(xL2$deviations, wave1=0, wave2=20)

#
# Wavelet scalogram of the L2 spiral deviations.
#

xL2wavelets <- computeDeviationWavelets(xL2)

#
# see text for explanation and help(computeDeviationWavelets)in logspiral
# ----------------------------------------------------
