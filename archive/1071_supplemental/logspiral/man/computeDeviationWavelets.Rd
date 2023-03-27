\name{computeDeviationWavelets}
\alias{computeDeviationWavelets}
\title{
Wavelet decomposition of spiral deviations
}
\description{
Computes a wavelet decomposition and plots the one with maximum variance
}
\usage{
computeDeviationWavelets(spiral.output = spiral.output, wave.levels = NULL, 
     print.var = TRUE, plot.var = TRUE, plot.maxWave = TRUE, units = "mm")
}
\arguments{
  \item{spiral.output}{
output from a spiral fit
}
  \item{wave.levels}{
number of levels (powers of 2) default is NULL see details
}
  \item{print.var}{
print variances at each level as a percentage of total variance
}
  \item{plot.var}{
plot the variance and confidence limits (see details)
}
  \item{plot.maxWave}{
wavelet decomposition column with maximum variance 
}
  \item{units}{
units to include on plot
}
}
\details{
Levels are the maximum power of 2 less than number of deviations. Actual levels are less than maximum because
of the default width and type of mother wavelet used(MODWT with least asymmetric filter, Percival & Walden 2000).
 
Variances and confidence limits as in Percival & Walden (2000) pages 311-315. The default scales have
units that are powers of 10. For example a 2^5 scale, would be log base 10 of 32 = 1.505. Same for variances.

Alternative is to plot variances from the wmtsa function wavVar and change the mother wavelet.
See example below, where less mother wavelet resolution provides some estimate at all scales 
possible (7 in this case). wavVar stops with an error message if you ask too many levels 
for the wavelet specified. 
}
\value{
data frame with rows the number of deviations and columns as values of each wavelet
in the decomposition
}
\references{

William Constantine and Donald Percival (2016). wmtsa: Wavelet
  Methods for Time Series Analysis. R package version 2.0-2.
  https://CRAN.R-project.org/package=wmtsa

Percival, D.B and Walden, A.T. Donald Percival (2000) Wavelet Methods for Time Series Analysis. Cambridge.

}
\author{
A.E. Aldridge
}
\note{
see the  \emph{wmtsa} help for \code{  wavMODWT}, \code{  wavMRD}, and \code{  wavVar} 
that are used in this function
}

\seealso{
   \code{ doCancelNoise} \code{    PeakCycle}
}
\examples{
\dontrun{
# a data frame of outline (x,y) coordinates based on a L1 spiral with six cycles
xy.df <- generateAnyLinear( waves=6) 
# default points generated is 200, so max wavelet scale is 2^7 = 128 
#
spiral.fit <- fitAnyLinear(xy.df$x, xy.df$y)  # fits the L1 spiral
## multi-resolution of deviations into wavelets
# will load the package wmtsa if not already loaded.
#
 dev.waves <- computeDeviationWavelets( spiral.fit ) 
# default will plot deviations along with max variance wavelet scale 
# in this case scale 5 or 2^5 = 32 units for the wavelength 
# default generation of L2 is 200 coordinates so 200/32 = 6 cycles
# 
## Check by plotting.  Wavelet scales 4 and 5 explains 90 percent
# of all spiral deviation variance. Verify by adding together wavelet
# columns 4 and 5 in the object dev.waves
#
plot ( spiral.fit$arc.length, spiral.fit$deviations, type = "s")
lines (spiral.fit$arc.length, dev.waves[ , 4] + dev.waves[ , 5], col= 4, lwd=1.5)
#
## alternative wavelet variance plotting using wmtsa function wavVar
# first the number scales used in computeWaveletDeviations,
# then the full levels using shorter, but less reliable mother wavelets
#
plot(wmtsa::wavVar(spiral.fit$deviations, wavelet = "s4", n.levels = 5))
plot(wmtsa::wavVar(spiral.fit$deviations, wavelet = "haar", n.levels = 7))
plot(wmtsa::wavVar(spiral.fit$deviations, wavelet = "s2", n.levels = 7))
# 
}
}
\keyword{ deviations } 
