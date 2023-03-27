\name{fitL2Spiral}
\alias{fitL2Spiral}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
fits TWO linear spirals (L2) at a specified change location
}
\description{
fits TWO linear spirals (L32) changing at ONE specified change point location
}
\usage{
fitL2Spiral(x, y, m = NULL, start.u = NULL, start.v = NULL, 
                start.values = c(a = 2, k1 = 0.2, k2 = 0.2), 
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
  \item{m}{
change location
}

  \item{start.u}{
start in x coord
}
  \item{start.v}{
start in y coord 
}
  \item{start.values}{
initial values for other unknowns in strict order of : 
distance to start of spiral (a), 
and two spiral angles (k1, k2) in radian units.
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
units for spiral angles (default degrees)
}
}
\details{
Called by \code{fitAnyLinear}, not meant as a standalone function.

Start values can be used to assist convergence when function used alone.

Uses exponential form of logarithmic spirals as input to \code{nls}
}
\value{
see \code{  fitAnyLinear}
}
\references{
Aldridge, A. (1999) Brachiopod outline and episodic growth. Paleobiology 25, 471-482
}
\author{
A.E. Aldridge
}
\note{
called by \code{  fitAnyLinear }
}

\section{Warning }{ 
See Troubleshooting Guide if not converging or an error

Axis start for the spiral requires input of location as two separate values.} 
