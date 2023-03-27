\name{fitL1Spiral}
\alias{fitL1Spiral}
\title{
fit a single linear (L1) logarithmic spiral
}
\description{
fit a single linear logarithmic spiral
}
\usage{
fitL1Spiral(x, y, start.u = NULL, start.v = NULL,
                start.values = c(a = 2, k = 0.2), 
                flip.x = FALSE, move.by = NULL, 
                tolerance.step = 0.01, angle.deg = TRUE)
}
\arguments{
   \item{x}{
x coord
}
  \item{y}{
y coord
}
  \item{start.u}{
start in x coord
}
  \item{start.v}{
start in y coord 
}
  \item{start.values}{
initial values for other unknowns in strict order of : 
distance to start of spiral (a), and spiral angle (k) in radian units.
}
  \item{flip.x}{
flip
}
  \item{move.by}{
move
}
  \item{tolerance.step}{
nls tolerance
}
 \item{angle.deg}{
units for the spiral angle (default is degrees)
}
}
\details{
Called by \code{fitAnyLinear}, not meant as a standalone function.

However start values can be input to help convergence, but need to use
radian units. 

Uses exponential form of logarithmic spiral as input to \code{nls}
}
\value{
see fitAnyLinear
}
\references{
Aldridge, A. (1999) Brachiopod outline and episodic growth. Paleobiology 25, 471-482
}
\author{
A.E. Aldridge
}
\note{
called by \code{  fitAnyLinear}, not meant to be a stand alone function.
}

\section{Warning }{ 
See troubleshooting guide if not converging or an error. 

Axis start for the spiral requires input of location as two separate values.
} 

\seealso{
\code{fitAnyLinear}
}
