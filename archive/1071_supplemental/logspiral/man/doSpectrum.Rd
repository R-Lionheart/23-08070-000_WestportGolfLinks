\name{doSpectrum}
\alias{doSpectrum}
\title{
plot the Fourier sums of squares against wave number (frequency)
}
\description{
Uses Fast Fourier Transform (fft) to estimate the sums of squares of 
each Fourier frequency from zero to N/2 (N = series length).
}
\usage{
doSpectrum(Series, W=1, wave1 = NULL, wave2 = NULL, Order = 2, Title = NA, output = FALSE)
}
\arguments{
  \item{Series}{
series of equally spaced values in space (eg arc length).
}
  \item{W}{
smoothing value, or the number of periodogram ordinates being averaged (W=1 no smoothing).
}
  \item{wave1}{
minimum Fourier frequency (wave number) to plot 
}
  \item{wave2}{
maximum Fourier frequency (wave number) to plot
}
  \item{Order}{
order of autoregressive (AR) model's spectrum to compare with actual spectrum
}
  \item{Title}{
title for plotting
}
  \item{output}{
output the spectrum (default is no output)
}
}
\details{
Equal spacing of values is assumed.  First frequency (wave1=0) is the variation about the
mean of the series, which should be zero. Apparent periodicity in a series can be generated 
by an AR process so pays to check if this is likely.  A very approximate confidence line is 
included to judge the significance (or not) of any peaks in a spectrum produced. Confidence limits
are based on spectrum terms being exponentially distributed (dashed lines).

Variance predicted for an AR process (default order is 2) is the solid (blue) line.


}
\value{
\item{output}{sums of squares (raw if W=1, smooth if W>1) at each wave number}
}
\references{
Thanks to Dr Robert Davies (Statistics Research Associates Ltd) kindly supplied most
of the code as an S-plus function, which the author ported to R.
}
\author{
A.E. Aldridge
}
\note{
Adapted from RB Davies Splus code. Any mistakes are the fault of logspiral package author.
}
\seealso{
\code{plotSpiralDeviations}     \code{    partitionSumSquares}
}
\examples{
\dontrun{
z <- generateAnyLinear( waves=6 )
xs <- fitAnyLinear(x=z$x, y=z$y)
doSpectrum( Series=xs$deviations, W=1, wave1=0, wave2=15, Title="L1 spiral with six cycles")
}
}
\keyword{ deviations }
