\name{compareFeaturesAB}
\alias{compareFeaturesAB}
\title{
Compares deviation features of TWO outlines, each fitted with a single (L1) spiral
}
\description{
Compares the single logarithmic spiral features of TWO outlines 
}
\usage{
compareFeaturesAB(spiralA = spiralA, spiralB = spiralB, print.it = FALSE)
}
\arguments{
  \item{spiralA}{
Deviation features of first outline (A).
}
  \item{spiralB}{
Deviation features of second outline (B)
}
  \item{print.it}{
print the distance between the two outlines
}
}
\details{
Local distance is based on Wavelet features (variance) and global 
distance is based on Fourier features (variance).
}
\value{
distance between two spirals: local and global.
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
A.E. Aldridge
}
\note{
Closest spirals in the catalogue can be created and tweaked using
the printed type of spiral and generating similar spirals ( see functions beginning
with 'generate' as in (for example) generateQuadraticLinear and generateQuadratic.
}
\section{Warning }{ Never a perfect match to input outline.  Check general pattern of 
the eleven spirals.  Also consider shapes NOT in the Catalogue (e.g double quadratic,
and bent cable to linear, LQL2) 
then compare deviations }
\seealso{
catalogueDeviationFeatures, computeDeviationsFeatures
}
\examples{
\dontrun{
za <- generateAnyLinear( k1=30,k2=30,k3=40,k4=40)
zb <- generateAnyLinear( k1=40,k2=45,k3=50,k4=50)
aspiral <- computeL1DeviationsFeatures(x=za$x, y=za$y)
bspiral <- computeL1DeviationsFeatures(x=zb$x, y=zb$y)
compareFeaturesAB(spiralA=aspiral, spiralB=bspiral)
}
}
