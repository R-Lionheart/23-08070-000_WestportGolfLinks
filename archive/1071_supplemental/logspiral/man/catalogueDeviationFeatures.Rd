\name{catalogueDeviationFeatures}
\alias{catalogueDeviationFeatures}
\docType{data}
\title{
Catalogue of 30 features from 838 instances of 11 types of logarithmic spiral.
}
\description{
Summary of features from fitting a single spiral to 838 different spirals
}
\usage{data(catalogueDeviationFeatures)}
\format{
  A data frame with 838 observations on the following 43 variables.
  \describe{
    \item{\code{seq}}{row number in Catalogue}
    \item{\code{k1}}{spiral angle 1 (degree units)}
    \item{\code{k2}}{spiral angle 2 (degree units)}
    \item{\code{k3}}{spiral angle 3 (degree units)}
    \item{\code{k4}}{spiral angle 4 (degree units)}
    \item{\code{dist}}{  the distance from axis to start of outline}
    \item{\code{c1}}{  location of 1st change as proportion of total outline points}
    \item{\code{c2}}{  location of 2nd change as proportion of total outline points}
    \item{\code{c3}}{  location of 3rd change as proportion of total outline points}
    \item{\code{quadratic}}{  the value of 2nd power of angle about axis}
    \item{\code{cubic}}{      the value of 3rd power of angle about axis}
    \item{\code{quartic}}{    the value of 4th power of angle about axis}
    \item{\code{type}}{numeric code for spiral type - used in sorting and plotting}
    \item{\code{W1}}{   Wavelet variance from highest (W1) to lowest (W10)frequency..}
    \item{\code{W2}}{   Wavelet variance   (W2)  }
    \item{\code{W3}}{   Wavelet variance   (W3)  }
    \item{\code{W4}}{   Wavelet variance   (W4)  }
    \item{\code{W5}}{   Wavelet variance   (W5)  }
    \item{\code{W6}}{   Wavelet variance   (W6)  }
    \item{\code{W7}}{   Wavelet variance   (W7)  }
    \item{\code{W8}}{   Wavelet variance   (W8)  }
    \item{\code{W9}}{   Wavelet variance   (W9)  }
    \item{\code{W10}}{  highest Wavelet frequency variance (N=1024)}
    \item{\code{F01}}{  Fourier variance from lowest (F01) to highest (F20) frequency...} 
    \item{\code{F02}}{  Fourier variance   (F02)  }
    \item{\code{F03}}{  Fourier variance   (F03)  }
    \item{\code{F04}}{  Fourier variance   (F04)  }
    \item{\code{F05}}{  Fourier variance   (F05)  }
    \item{\code{F06}}{  Fourier variance   (F06)  }
    \item{\code{F07}}{  Fourier variance   (F07)  }
    \item{\code{F08}}{  Fourier variance   (F08)  }
    \item{\code{F09}}{  Fourier variance   (F09)  }
    \item{\code{F10}}{  Fourier variance   (F10)  }
    \item{\code{F11}}{  Fourier variance   (F11)  }
    \item{\code{F12}}{  Fourier variance   (F12)  }
    \item{\code{F13}}{  Fourier variance   (F13)  }
    \item{\code{F14}}{  Fourier variance   (F14)  }
    \item{\code{F15}}{  Fourier variance   (F15)  }
    \item{\code{F16}}{  Fourier variance   (F16)  }
    \item{\code{F17}}{  Fourier variance   (F17)  }
    \item{\code{F18}}{  Fourier variance   (F18)  }
    \item{\code{F19}}{  Fourier variance   (F19)  }
    \item{\code{F20}}{  highest Fourier variance (N=1024)}
  }
}
\details{
 The first 13 columns describe the type of spiral shape, next ten columns are the
 wavelet variances (using \strong{wmtsa} package and defaults for 1024 observations).
 The remaining 20 columns are Fourier spectrum variances using base R's \strong{fft} function with the same N=1024.
}
\source{
The \strong{generate} functions of logspiral were used to output a shape (NOT equal spaced coordinates).
Then the single spiral fitting was applied to extract the deviations, which were then interpolated to 
1024 and scaled to 10 vertical units and 100 horizontal (arc length) units.
}
\references{
William Constantine and Donald Percival (2016). wmtsa: Wavelet
  Methods for Time Series Analysis. R package version 2.0-2.
  https://CRAN.R-project.org/package=wmtsa

Percival, D.B and Walden A.T. Donald Percival (2000) Wavelet Methods for Time Series Analysis. Cambridge.
}
\examples{
data(catalogueDeviationFeatures)
#
# print (to screen) the first four rows and 13 columns in the data frame
catalogueDeviationFeatures[1:4, 1:13]
#
# table the types (numeric code)of spiral in the catalogue
table(catalogueDeviationFeatures$type)
#
# total number of instances of spirals in the catalogue
sum(table(catalogueDeviationFeatures$type))

}
\keyword{datasets}
