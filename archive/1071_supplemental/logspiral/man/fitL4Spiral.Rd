\name{fitL4Spiral}
\alias{fitL4Spiral}
\title{
fits FOUR linear spirals (L4) at specified change locations
}
\description{
fits FOUR linear spirals (L4) changing at THREE specified change point locations
}
\usage{
fitL4Spiral(x, y, m = c(NULL, NULL, NULL), start.u = NULL, start.v = NULL,
           start.values = c(a = 2, k1 = 0.2, k2 = 0.2, k3 = 0.2, k4 = 0.2), 
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
given change points
}
  \item{start.u}{
start location x
}
  \item{start.v}{
start location y 
}
  \item{start.values}{
initial values for other unknowns in strict order of : distance to start of spiral (a), and four spiral angles (k1, k2, k3, k4) in radian units.
}
  \item{flip.x}{
flip
}
  \item{move.by}{
move
}
  \item{tolerance.step}{
tolerance for nls
}
 \item{angle.deg}{
units for spiral angle output (default is TRUE)
}
}
\details{
Called by \code{  fitAnyLinear}, not meant as a standalone function.

Start values may assist convergence when function is used alone.

Axis start needs input as two separate values.

Uses exponential form of logarithmic spirals as input to \code{nls}
}
\value{
see \code{  fitAnyLinear}
}
\references{
Aldridge,A. (1999) Brachiopod outline and episodic growth. Paleobiology 25, 471-482
}
\author{
A.E. Aldridge
}
