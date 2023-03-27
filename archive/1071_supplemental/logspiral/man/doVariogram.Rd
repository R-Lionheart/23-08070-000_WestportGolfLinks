\name{doVariogram}
\alias{doVariogram}
\title{
produces a semi-variogram to highlight trends or cycles in a series
}
\description{
Estimates variation based on increasing differencs (lags) between values.
}
\usage{
doVariogram(Series, nlags = NULL, plot.it = T, output = F, Title = NA)
}
\arguments{
  \item{Series}{
 N equally spaced values 
}
  \item{nlags}{
number of lags to compute (maximum is N/2)
}
  \item{plot.it}{
plot the semi-variogram (default)
}
  \item{output}{
output the results (default is no output)
}
  \item{Title}{
title for the semi-variogram
}
}
\details{
Variation as standard deviation and variance is estimated for each lag (from 1 to N/2).
The standard deviation is plotted against increasing lag. If output is TRUE then lag number
standard deviation, variance, and number of values for each lag are combined into a data frame.
As lag number increases there are fewer values for which to estimate variation.

Warning: assumes no missing values
}
\value{
\item{output}{data frame with: lag, standard deviation, variance, number of values}
}
\references{
Smith P.L. 2001.  A primer for sampling solids, liquids and gases. SIAM, ASA.
}
\author{
A.E. Aldridge
}
\note{
Code for R adapted from Visual Basic code, see Table D.1 of Smith (2001)
}

\seealso{
\code{ plotSpiralDeviations}    \code{     doSpectrum}      \code{    doLocateChange}
}
\examples{
\dontrun{
z <- generateAnyLinear()
xs <- fitAnyLinear(x=z$x, y=z$y, waves=6)
doVariogram( Series=xs$deviations, Title="L1 with six cycles" )
# should see three peaks and troughs up to N/2 lags
}
}

\keyword{ deviations }
