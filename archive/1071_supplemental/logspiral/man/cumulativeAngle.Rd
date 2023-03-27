\name{cumulativeAngle}
\alias{cumulativeAngle}
\title{accumulates angle (radians)
}
\description{accumulates angle (radians) along an outline about a fixed point 
}
\usage{
cumulativeAngle(x, y, u, v)
}
\arguments{
  \item{x}{ x coordinate
}
  \item{y}{ y coordinate
}
  \item{u}{ x coordinate of fixed point such as a spiral axis location
}
  \item{v}{ y coordinate of fixed point such as a spiral axis location
}
}
\details{
Checks whether coordinate sequence is regressing and changes sign accordingly.
Function used as part of spiral fitting see fitAnyLinear.  

Angle starts close to zero ( at angle eps = 0.00001). 
}
\value{
\item{theta }{  the accumulated angle (radians) about the fixed point (u,v)}
}
\references{

https://en.wikipedia.org/wiki/Dot_product
}
\author{
A.E. Aldridge
}
\note{
Beware zig-zags when points are dense: don't overdo the digitising.  Useful to
print and plot detail in the outline.  Drawing segments from fixed point to
outline can verify that the accumulated angle is correct.
}
\section{Warning }{ zig-zags or growth regressing in an accretionary outline } 

\seealso{
\code{ rotate2D}   \code{   fitAnyLinear}         \code{   equalSpace}  
}
\examples{
\dontrun{
MASS:: eqscplot( c(4,0,-3,1), c(-5,5,10,15), pch=c("1","2", "3", "4"))
 points(6,4, pch=10, cex=2)

# clockwise produces negative angles
cumulativeAngle(  c(4,0,-3,1), c(-5,5,10,15), u=6,v=4)
# anti-clockwise has positive angles
cumulativeAngle(  rev(c(4,0,-3,1)), rev(c(-5,5,10,15)), u=6,v=4)
}

}



