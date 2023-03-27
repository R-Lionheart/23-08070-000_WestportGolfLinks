\name{exSpirulaBeak}
\alias{exSpirulaBeak}
\docType{data}
\title{
Rams horn shell: Spirula spirula
}
\description{
beak of squid \emph{Spirula spirula}
}
\usage{data(exSpirulaBeak)}

\format{
  A data frame with xx observations on the following 3 variables.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
    \item{\code{rostrum}}{a character vector}
    \item{\code{side}}{a character vector}
  }
}
\details{
This beak from same specimen of \emph{Spirula} in the file \bold{exSpirulaShell}
}
\source{
Image from Dr. Mark Norman (Dec. 2003)  Museum Victoria, Melbourne, Australia
}
\references{
http://tolweb.org/Spirula_spirula/19989  
(accessed 20 Nov. 2019)
}
\examples{
\dontrun{
## create some graphic windows ( 2 to 5)
aaSetupGraphics()
## load the data
data(exSpirulaBeak)
## extract the right outer (upper) beak and plot
z.df <- exSpirulaBeak
zright.df <- z.df[z.df$rostrum=="outer" & z.df$side=="right", ]
x <- zright.df$x
y <- zright.df$y
MASS::eqscplot(x,y)
## fit a single spiral
zspiral <- fitAnyLinear( x,y, plot.deviations=T, plot.fit=T)
## now see if there is a spiral change
zchk <- fitCheckL2Spiral( x,y, plot.detail=F)
## fit at the change point
zspiral <- fitAnyLinear( x,y, m=c(88), plot.deviations=T, plot.fit=T)
## noise cancel the deviations 
znoise.cancel <- NoiseCancelDeviations( x,y, zspiral)
}

}
\keyword{datasets}
