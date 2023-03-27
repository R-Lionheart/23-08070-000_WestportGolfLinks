\name{generateAnyLinear}
\alias{generateAnyLinear}
\title{
generate up to FOUR connected spirals as a single outline
}
\description{
generate up to four spirals using three specified spiral angles
}
\usage{
generateAnyLinear(nstart = 200, change = c(50, 100, 150), rate = 0.4, 
                  k1 = 35, k2 = 35, k3 = 35, k4 = 35, dist.zero = 2, 
                 noise = 0.01,noise.increase = FALSE, orient = "NO", 
                 distance = "far", angle = 0, plane = "X", plot = T, 
                 waves = 1, show.changes = TRUE, theta.start = 0.001)
}

\arguments{
  \item{nstart}{
number of outline points to generate (default 200)
}
  \item{change}{
where spirals change along the outline (default at the 50th, 100th and 150th 
}
  \item{rate}{
rate at which the spiralhe rate spiral is grown (see details)
}
  \item{k1}{
spiral angle 1  ( note if all equal then only one spiral episode is generated)
}
  \item{k2}{
spiral angle 2  ( note if all equal then only one spiral episode is generated)
}
  \item{k3}{
spiral angle 3  ( note if all equal then only one spiral episode is generated)
}
  \item{k4}{
spiral angle 4  ( note if all equal then only one spiral episode is generated)
}
 \item{dist.zero}{
initial distance between axis and spiral when angle (theta) is zero 
}
  \item{noise}{
standard deviation of error added to the distance from spiral axis
}
  \item{noise.increase}{
does error increase LINEARLY from umbo, default is no increase
}
  \item{orient}{
orientation 
}
  \item{distance}{
distance to translate the outline
}
  \item{angle}{
angle to orient the spiral OUT OF PLANE
}
  \item{plane}{
to orient the spiral outline ( "X" or "Y")
}
  \item{plot}{
plot
}
  \item{waves}{
waves or cycles in the deviations
}
\item{show.changes}{
mark the outline locations where spirals change
}
\item{theta.start}{
the initial value for starting angle (to avoid zero)
}
}
\details{
Generates a spiral based on constant arc length increment (rate). Other function
arguments are used to test fitting on a range of variations to an underlying
spiral or spirals.  For example, adding cycles to the growth, or twisting the 
outline out of the X-Y plane (outline coordinates are projection on X-Y plane).  

Cycles of fixed amplitude and phase are fixed by the \bold{waves} argument.
}
\value{
\item{xy}{ data frame with spiral coordinates as x, y and logr, angle}
}
\references{

Aldridge (1999) Brachiopod outline and episodic growth. Paleobiology 25, 471-482

}
\author{
A.E.  Aldridge
}
\note{
Small spiral angles (eg k= 30) generate a 'flat' outline vs whorls when large spiral angle 
(eg k = 80).   Spiral angles in degrees are converted to radians.  A summary is printed
on the console.
}
\section{Warning }{may need to trial and error to find best orientation 
            plotting (TRUE) is required for obtaining output data frame} 
\seealso{
Giant squid data for example of small spiral angle and
vignettes on generating and fitting linear spirals.
}
\examples{
\dontrun{
doSetupGraphics()
#
# One linear (L1) logarithmic spiral with default of 200 near equally spaced coordinates
#
 z1 <- generateAnyLinear()
#
# Two linear (L2) connected spirals with default 200 points, spiral change 100 coord from umbo
# first spiral angle is 30 degrees, second is 40 degrees
#
  z2 <- generateAnyLinear( k1=30, k2=30, k3=40, k4=40,  noise=0.001)
# 
dev.set(3)  # plot on device 3 the angle about axis and log of radius
plot( z2$angle, z2$logr) # a broken stick line
dev.set(4)
plot (z2$x, z2$y,pch=16, cex=0.3) # the spiral outline 
#
# Three linear spirals (L3)  default 200 points,changes at 100,150 coordinates from umbo
#
z3 <- generateAnyLinear( k1=35,k2=35,k3=40,k4=55, dist.zero=5) 
#
# Four linear spirals (L3)  default 200 points, changes at 50, 100,150 coorinate from umbo
#
z4 <- generateAnyLinear(  k1=35,k2=40,k3=45,k4=55)
#
}
}
