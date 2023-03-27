\name{StartAngle}
\alias{StartAngle}

\title{
find the start angle of a sequence of outline points
}
\description{
find start angle of outline relative to given point (axis) u, v
}
\usage{
StartAngle(xc, yc, u1, v1)
}

\arguments{
  \item{xc}{
x coordinate sequence
}
  \item{yc}{
y coordinate sequence
}
  \item{u1}{
x coordinate of fixed point
}
  \item{v1}{
y coordinate of fixed point
}
}
\details{
This function can be streamlined by using \code{atan2} and one
\code{ifelse}

\code{ angle <- atan2( yc-v1, xc-u1)}

\code{angle <- ifelse( angle < 0, angle + 2*pi, angle)}
}
\value{
\item{start.theta}{ starting angle in radians }
}

\author{
A.E. Aldridge
}

\examples{
##data( GiantSquidBeak)

## The function is currently defined as
function (xc, yc, u1, v1) 
{
    start.theta <- atan(abs((yc - v1))/abs(xc - u1))
    if (xc - u1 < 0 && yc - v1 > 0) 
        start.theta <- pi - start.theta
    else if (xc - u1 < 0 && yc - v1 < 0) 
        start.theta <- pi + start.theta
    else if (xc - u1 > 0 && yc - v1 < 0) 
        start.theta <- 2 * pi - start.theta
    return(start.theta)
  }
}
