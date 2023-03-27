\name{logspiral-package}
\alias{logspiral-package}
\alias{logspiral}
\docType{package}
\title{Fits a range of spirals to a sequence of outline coordinates in two dimensions(2D)}
\description{ Fits \strong{log-piecewise} and \strong{log-polynomial} spirals to an outline. Deviations are extracted from each spiral fit.

Outline smoothing provides alternative non-spiral based deviations.  Also available is a function that can generate up to four connected or log-piecewise spirals with user-specified spiral angles, change locations, noise and the number of cycles. Other functions can generate user-specified curved or nonlinear spirals. Generated spirals are useful for investigating spiral fits and resulting deviations against actual outline data. 

A spiral for an unknown outline can be indicated from a catalogue of twelve types of spiral (log-piecewise and log-curved). }
\details{
Fitting uses the base R function \strong{nls} with the spiral(s) as exponential equations for minimisation of 
residual sums of squares. Other optimisers have been investigated (eg nlxb, ms, optim), but \strong{nls} remains reliable when initial parameter values are near to the optimum. 

Alternative methods for optimisation are Monte Carlo and particle swarm.  However, Monte Carlo (JAGS, STAN) is not yet possible because independent variables also need to be updated as the axis location is changed when searching for an optimum.  Particle swarms similarly fail but mght be are better for model simulations in a designed search (e.g. see the Latin hypercube in \code{hydroPSO}).

Developers, please note that functions in logspiral have been mostly created and tested as standalone code. So the names of variables are not always consistent between functions.
There has been the convention of prefixing temporary or working variables with the letters x, xx, yy, z or zz. The resulting code is sometimes not pretty and should be condensed in later versions of logspiral. These variable names are different from the usual 2D coordinate names of x,y. 

Output from all fitting is in terms of intercept and slope on a logarithmic scale of radius versus angle about an estimated axis location. The outline must start near an umbo (zero angle) and turns anticlockwise for the slope to be
positive. Outlines turning clockwise are flipped to be anticlockwise along with a warning message.  Once warned, you should flipping and translate your outline coordinates before any final spiral fit. A clockwise turning outline with all or some negative coordinates may not produce a sensible fit.

The slope (also known as the expansion rate of a spiral) is reported as spiral angle (k) in radian units, which by default is converted to degrees as (180/pi)*atan(1/k). Degree units are more easily interpreted. A spiral angle of 90 degrees is a circle, while an spiral angle of zero degrees is a straight line. The spiral expansion rate as a spiral angle parameter must not be confused with angle turned about the spiral axis.

Spiral angle has also been reported as a whorl expansion (W) in Raup (1966), and in McGhee (1980). Spiral angle (k) in radians converts to the whorl expansion in natural logarithm units with multiplication by \code{2*pi}
( i.e. assigment as an object, lnW:  \code{lnW <-  2*pi*k)}.  


\tabular{ll}{
Package: \tab logspiral\cr
Type: \tab Package\cr
Version: \tab 0.9.6\cr
Date: \tab 2020-08-08\cr
License: \tab GPL (>= 2)\cr
}
\code{  generateAnyLinear}  \code{ fitAnyLinear}    \code{ fitCheckL2Spiral } are key functions to demonstrate this package.  
 \code{doSetupGraphics} opens four graphic windows, or alternatively, repeat \code{dev.new()} four times to display results. Examples throughout assume four graphic windows are open as devices 2, 3, 4, and 5. 
}
\author{A.E. Aldridge

Maintainer: A.E. Aldridge <tony@southnet.co.nz>
}
\references{
Aldridge (1998) Brachiopod outline and the importance of the logarithmic
spiral. Paleobiology 24, 215-226

Aldridge (1999) Brachiopod outline and episodic growth. Paleobiology 25, 471-482

Aldridge (2009) Can beak shape help to research the life history of squid?  
New Zealand Journal of Marine and Freshwater Research 43  1061-1067

Aldridge & Gaspard D. (2011) Brachiopod life histories from spiral deviations in shell
shape and microstructural signature - preliminary report. Memoirs of the Association
of Australasian Paleontologists 41, 256-268

Clark J., et al (2015) Application of shell spiral deviations methodology to fossil brachiopods. 
Palaeontologica Electronica 18.3.54A:1-39

Gaspard D., Aldridge et al (2018) Analysis of growth and form in \emph{Aerothyris kerguelenensis} 
(rhynchonelliform brachiopod) - Shell spiral deviations, microstructure, trace element contents and 
stable isotope ratios. Chemical Geology, 483: 474-490

McGhee, G.R.(1980) Shell form in the biconvex articulate Brachiopoda:
a geometric analysis. Paleobiology 6, 57-76.

Perez-Huerta A., et al (2014) Brachiopod shell spiral deviations (SSD): implications for
trace element proxies. Chemical Geology 374-375, 13-24.

Raup D.M. (1966) Geometric analysis of shell coiling: general problems. Journal of Paleontology, 40(5), 1178-1190.
                  


} 
