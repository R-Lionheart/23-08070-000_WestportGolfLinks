\name{compareSpiralFits}
\alias{compareSpiralFits}
\title{
Summarises and compares spirals on partitioned variation  
}
\description{
Compares up to five fitted spirals using three partitions of variation (see details)
}
\usage{
compareSpiralFits(s1 = NULL, s2 = NULL, s3 = NULL, s4 = NULL, s5 = NULL, ratios = TRUE)
}
\arguments{
  \item{s1}{
output from first spiral
}
  \item{s2}{
output from second spiral
}
  \item{s3}{
output from third spiral
}
  \item{s4}{
output from fourth spiral
}
  \item{s5}{
output from fifth spiral
}
  \item{ratios}{
should a table of ratios be produced (default TRUE)
}
}
\details{
Ratios only computed and output if TWO or more spirals are included.

Sums of squares about zero spiral deviation are estimated at each Fourier
frequency.  Estimation is through the fast Fourier transform when calling 
the function \code{partitionSumsSquares}.

Variation is partitioned as  low frequency,  growth periodicity, and high frequency.
Percentages of total variation is tablulated for all spirals.  Low frequency combines the
Fourier frequencies 1 to 4, periodicity combines 5 to 19 and the high frequency from 
20 to the the maximum ( n/2). 

If the low frequency partition is more than half the total variation then 
the fitted spiral is probably the wrong type for the outline.

If ratios=TRUE, then spiral partitions are divided in the order input.  So the first 
ratio will be spiral s1 compared to s2  as a ratio for each of the three partitions.
}
\value{
two tables (if ratios=TRUE) with percentage variation 
}
\references{
 no references yet
}
\author{
A.E.  Aldridge
}
\note{
when only one spiral is input then only percentages of total variation are output.
}

\seealso{
partitionSumsSquares
}
\examples{
\dontrun{

# generate and fit a spiral using defaults
#
z <- generateAnyLinear()
xs <- fitAnyLinear(x=z$x, y=z$y)
#
# a single (L1) spiral with 35 degree spiral angle
# using function shows that 80% of variance in spiral 
# deviations is in the high frequency components. 
#
compareSpiralFits(xs)
#
# Now generate a L2 spiral with change at 100th coordinate
# then fit a L1 spiral and a L2 spiral to this outline.
#
z <- generateAnyLinear(k1=35, k2=35, k3=40, k4=40)
xs1 <- fitAnyLinear(x=z$x, y=z$y)
xs2 <- fitAnyLinear(x=z$x, y=z$y, m=100)
#
# Compare deviations from fitting L1, L2 spirals to the same
# outline data (z)
#
compareSpiralFits(s1 = xs1, s2 = xs2)
#
# Shows that xs1 (the L1 fit) has 73 percent of variance in the low
# frequencies so this is not a good fit, xs2 has only 1.7 percent.
# Periodicity (cyclic) frequencies exhaust ~ 15 to 18 percent of 
# variation and deemed part of the random or noise component
# along with high frequency components.
}
}

\keyword{deviations}
