\name{doSetupGraphics}
\alias{doSetupGraphics}
\title{ set up  graphics windows

}
\description{
set up FOUR graphic windows numbered 2, 3, 4 and 5
}
\usage{
doSetupGraphics( high=4, wide = 4, pt.size = 8)
}

\arguments{
  \item{high}{
height of window in points
}
  \item{wide}{
width of window in points
}
  \item{pt.size}{
number of points per unit of screen display
}
}

\details{
Graphic windows are located assuming a wide screen with only the R command window on the left.
Height and width and point size can be altered. Subsequent manual resizing of windows can
distort placement of graphics.
}
\value{
Four graphic windows with the last one the active window.
}

\author{
A.E. Aldridge
}
\note{
Need to manually copy and edit function if changing location or size of 
graphic windows. Create your own graphics function (e.g. \code{mySetupGraphics}) to set up four graphic windows
for your own display device.
}
\seealso{
\code{dev.set}   \code{     dev.list}  \code{     dev.next}  \code{     dev.new} in base R.
}
\examples{
##doSetupGraphics()
## return to the first graphics window
dev.set(2)

## The function is currently defined as
function (high = 4, wide = 4, pt.size = 8) 
{
    windows(height = high, width = wide, pointsize = pt.size, 
        xpos = -25, ypos = 0)
    windows(height = high, width = wide, pointsize = pt.size, 
        xpos = -450, ypos = 0)
    windows(height = high, width = wide, pointsize = pt.size, 
        xpos = -25, ypos = -30)
    windows(height = high, width = wide, pointsize = pt.size, 
        xpos = -450, ypos = -30)
    invisible()
  }
}

\keyword{ display }
