\name{computeL1DeviationFeatures}
\alias{computeL1DeviationFeatures}
\title{
Computes local and global features of deviations from a single spiral
}
\description{
Computes 10 local and 20 global features of deviations from a single spiral
}
\usage{
computeL1DeviationFeatures(x = x, y = y, plot.it = T)
}
 
\arguments{
  \item{x}{
x coordinate of outline 
}
  \item{y}{
y coordinate of an outline
}
  \item{plot.it}{
plot (default) the deviations
}
}
\details{
10 local features are the wavelet variances, and 20 global features are variances from the
first twenty Fourier frequencies. First frequency is the variance around mean, which should be zero.

The 1024 interpolated (equal arc length) deviations are reduced to ten wavelet scales.
Fourier spectrum is computed using the base R fast Fourier transform (fft) while
the wavelet spectrum (also called scalogram) uses the R package wmtsa (Constantine and 
Percival, 2011). Wavelet scalogram applies the default maximal overlap, discrete 
transform (MODWT) with the default least asymmetric filter (Percival and Walden, 2000). 
}
\value{
numeric vector of 30 values
}
\references{
William Constantine and Donald Percival (2016). wmtsa: Wavelet
  Methods for Time Series Analysis. R package version 2.0-2.
  https://CRAN.R-project.org/package=wmtsa

Percival, D.B and Walden, A.T. Donald Percival (2000) Wavelet Methods for Time Series Analysis. Cambridge.

Fast Fourier Transform (\code{fft}) in base R.  See \code{help(fft)}
}
\author{
A.E. Aldridge
}
\note{
Wavelet features seem the most useful. Might only need ten (not 20) Fourier frequencies.
}
\seealso{
\code{wmtsa::wavMODWT}
\code{doSpectrum}

doVariogram
}
\examples{
\dontrun{
xy <- generateAnyLinear(k1=30, k2=30, k3=40, k4=40)
z <- computeL1DeviationFeatures(x=xy$x, y=xy$y)
compareFeaturesToCatalogue(z)
}

}
