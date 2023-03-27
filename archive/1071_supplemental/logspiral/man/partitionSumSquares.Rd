\name{partitionSumSquares}
\alias{partitionSumSquares}
\title{
Partitions the variation in a sequence of N equally spaced spiral deviations 
}
\description{
Partitions the variation of spiral deviations for each of N/2 Fourier frequencies
}
\usage{
partitionSumSquares(spiral.input = spiral.input, plot.it = T, print.output = T)
}
\arguments{
  \item{spiral.input}{
results from a spiral fit are used as input
}
  \item{plot.it}{
should variation from Fourier frequencies be plottted (default is yes).
}
  \item{print.output}{
should variation from Fourier frequencies be printed (default is yes).
}
}
\details{
Uses fast Fourier transform (fft in base R) to partition variance (power) at
N/2 Fourier frequencies.  

Deviations are equally spaced against arc length before applying fft.

The output value is sums of squares with a total that should closely match
(when  multiplied by N-1 and square root of the result)the standard deviation 
(sigma) of the spiral fit 

}
\value{
a vector of N/2 sums of squares of deviations about zero.
}
\references{
 no reference yet
}
\author{
A.E. Aldridge
}
\note{
Used in the function \code{  compareSpiralFits}
}

\seealso{
\code{compareSpiralFits}
}
\examples{
\dontrun{
z <- generateAnyLinear()
xs <- fitAnyLinear(x=z$x, y=z$y)
xsum.squares <- partitionSumSquares(xs)
}
}
\keyword{deviations} 
 
