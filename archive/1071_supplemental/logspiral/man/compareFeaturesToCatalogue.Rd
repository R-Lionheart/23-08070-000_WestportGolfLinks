\name{compareFeaturesToCatalogue}
\alias{compareFeaturesToCatalogue}
\title{
Compares deviation features of an outline to a catalogue of spirals.
}
\description{
Compares features of an outline to features to a catalogue then outputs the
nearest NINE spirals (ranked) as a prior choice for fitting the outline.
}
\usage{
compareFeaturesToCatalogue(spiral.features = spiral.features,  print.it = FALSE, 
                    plot.detail = FALSE,output.results=FALSE, 
                    graphic.windows = c(2, 3, 4, 5))
}
\arguments{
  \item{spiral.features}{
the output from fitting a single log spiral to an outline
}
  \item{print.it}{
print summary of NINE nearest spirals
}
  \item{plot.detail}{
plot the Wavelet (local) and Fourier (global) detail in separate windows 
}
  \item{output.results}{
list of the nine closest local (Wavelet) and nine closest global (Fourier) distances
}
  \item{graphic.windows}{
graphics windows to use in the plotting 4 = default output, 
detail in 3 = Wavelet (all), 2 = Fourier (all), 5 = local & global ratios for ranked spirals
}
}
\details{
Default output is a graph of local (Wavelet) and global (Fourier) features in order
of closest fit.  Measure of closeness is the symmetric ratio of Choi et al (2008).
}
\value{
\item{wratio.diff.sum}{sum of local, Wavelet differences}
\item{fdiff.sum}{  sum of global, Fourier differences}
}
\references{
Choi, H., Ombao, H., Ray, B. (2008). Sequential Change-Point Detection Methods for 
Nonstationary Time Series. Technometrics, 50(1):40-52.


William Constantine and Donald Percival (2016). wmtsa: Wavelet
  Methods for Time Series Analysis. R package version 2.0-2.
  https://CRAN.R-project.org/package=wmtsa

Percival, D.B and Walden A.T. Donald Percival (2000) Wavelet Methods for Time Series Analysis. Cambridge.
}
\author{
A.E.  Aldridge
}
\note{
Closest spirals in the catalogue can be created and tweaked using
the printed type of spiral and generating similar spirals ( see functions beginning
with 'generate' as in (for example) generateQuadraticLinear and generateQuadratic.
}
\section{Warning }{ Never a perfect match to input outline because of periodicities
in actual growth.  Check the general pattern of the nine ranked spirals.  
Also consider shapes NOT in the Catalogue (e.g. double quadratic, bent cable to linear LQL2). 
 }
\seealso{
\code{catalogueDeviationFeatures   	computeDeviationFeatures  	compareFeaturesAB}
}
\examples{
\dontrun{
z <- generateAnyLinear( k1=40, k2=45, k3=50, k4=60) # four spirals
zfeatures <- computeL1DeviationFeatures( x = z$x, y=z$y)
compareFeaturesToCatalogue(zfeatures)
compareFeaturesToCatalogue(zfeatures, plot.detail=TRUE)
zout <- compareFeaturesToCatalogue(zfeatures, plot.detail=TRUE, output.results=TRUE)
}
}
\keyword{ deviations } 
