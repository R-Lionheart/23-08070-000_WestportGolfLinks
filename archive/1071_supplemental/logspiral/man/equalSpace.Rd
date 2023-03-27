\name{equalSpace}
\alias{equalSpace}
 
\title{
equal space outline coordinates using arc length
}
\description{
equal spacing along an outline coordinates using arc length
}
\usage{
equalSpace(x, y, nequal = 50, light.smooth = FALSE)
}

\arguments{
  \item{x}{
x coord
}
  \item{y}{
y coord
}
  \item{nequal}{
number of equally spaced points required
}
  \item{light.smooth}{
light smooth (default is none )
}
}
\details{
 Uses linear interpolation. Do check the effect on an outline
 by plotting a few pointsof the actual and different numbers of
 equal spaced coordinates. Double or quadruple the actual number 
 of digitised points often works well when NO zig-zags in the growth outline.
}
\value{
\item{x,y}{ coordinates of equally spaced points using arc length}
\item{arc.length}{arc length along the sequence of output x, y values}
}
\references{
Ramsay and Silverman (2002)  Applied Functional Data Analysis 

Section 8.2.  Analysing shapes without landmarks ( notchproc2.fun)

http://www.stats.ox.ac.uk/~silverma/fdacasebook/  (accessed 24 Sept. 2019)

}
\author{
A.E. Aldridge using code from Ramsay & Silverman (2002) online resources.
}
\note{
If very few points then nequal needs to match the input with an even,or odd total points
}

\examples{
\dontrun{
x <- c(1,4,6,7,8,9)
y <- c(2.3, 4.5, 2, 6.5, 8.5,5.5)
#
MASS::eqscplot(x,y,type="b")
same.pts <- equalSpace(x,y,nequal=6)
same.pts.smooth <- equalSpace(x,y,nequal=6,light.smooth=TRUE)
dense.pts <- equalSpace(x,y, nequal=14)
#
MASS::eqscplot(x,y,type="b")
points( same.pts$x, same.pts$y, type="b",pch=16)
eqscplot(x,y,type="b")
points( same.pts.smooth$x, same.pts.smooth$y, type="b",pch=17)
#
MASS::eqscplot(x,y,type="b")
points( dense.pts$x, dense.pts$y, type="b",pch=15)
}
}

