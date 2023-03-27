\name{rotate2D}
\alias{rotate2D}

\title{
rotate 2D coordinates CLOCKWISE about a given location.
}
\description{
rotate 2D coordinates clockwise
}
\usage{
rotate2D(x, y, theta, u = 0, v = 0)
}
\arguments{
  \item{x}{
x coord
}
  \item{y}{
y coord
}
  \item{theta}{
angle to rotate (radians)
}
  \item{u}{
x coord about which to rotate
}
  \item{v}{
y coord about which to rotate
}
}
\details{
theta is in radian units, input theta as \code{ (pi/180)*angle} if angle is
in degree units
}
\value{
rotated coordinates returned as a data frame
}
\references{
Maxwell EA (1958)  Elementary Coordinate Geometry
}
\author{
A.E. Aldridge
}
\examples{
##data(Laqueus)

## The function is currently defined as
function (x, y, theta, u = 0, v = 0) 
{
# theta in radians
    xx <- x - u
    yy <- y - v
    xp <- xx * cos(theta) + yy * sin(theta)
    yp <- -xx * sin(theta) + yy * cos(theta)
    output <- data.frame(x = xp + u, y = yp + v)
    return(output)
}
}
