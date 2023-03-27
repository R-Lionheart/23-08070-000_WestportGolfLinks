fitLinear2BentCable <-
function (x, y,   m = c(NULL,NULL), axis.start = c(NULL,NULL), quadratic=TRUE,
start.values = c(a0=2,k0=36,k1=40,k2=55, g=0.3, kappa=2.0),  
  angle.deg = TRUE, flip.x = FALSE, move.by = NULL,
  tolerance.step = 0.1, trace.check=FALSE,
 plot.fit=FALSE, plot.deviations=FALSE, plot.diagnostics=FALSE,
title.label="fit to Linear to Bent Cable outline ") 
{
#
# Linear to Linear-Quadratic-Linear with TWO given transitions points
# the quadratic has symmetry at second transition
# Not yet the generalised bent cable of Khan and Kar (2018)
#
# start values are in the order of the linear, two linear of bent cable and g
# a0, k0, k1, k2, and g
# second & third intercepts (a1,a2) by calculation, as for second slope (k2)
#
    lspiral.lhs <- function(x, y, u, v) sqrt((x - u)^2 + (y - v)^2)
#
if(quadratic)
{
    lspiral.rhs.q <-  function(x, y, u, v,a0,k0, k1, kdiff,g ) {
        theta <- logspiral::cumulativeAngle(x, y, u, v)
#   m1, m2 declared in main funtion body
        xc1 <- theta[m1]
        xc2 <- theta[m2]        #  xc2 is tau (Chiu)
theta.d <-  (theta - xc2)  # theta centred at tau  
#
      quad.linear <- ((theta.d + g)^2/(4 * g)) * 
            (theta.d >= -g & theta.d <= g) + theta.d * (theta.d >  g)
#
 a0*exp( k0*pmin(theta,xc1)+  k1*pmax(0,theta-xc1) + kdiff *quad.linear) # 
    }
}
else
{
    lspiral.rhs.g <-  function(x, y, u, v,a0,k0, k1, kdiff,g,kappa ) {
        theta <- logspiral::cumulativeAngle(x, y, u, v)
#   m1, m2 declared in main funtion body
        xc1 <- theta[m1]
        xc2 <- theta[m2]        #  xc2 is tau  of Chiu (2006)
        tau <- xc2
theta.d <-  (theta - xc2)  # theta centred at tau  
#

    quad.linear.part <- ifelse( kappa !=2 & (theta.d + (kappa-1)*g) <= 0, 0,
                         (g*((theta.d +(kappa-1)*g)^kappa)/(kappa*g)^kappa)  )
#
    quad.linear <- quad.linear.part*(theta > (tau-(kappa-1)*g) & theta < tau+g ) + 
                        (theta.d)*(theta > tau+g)  
#
 a0*exp( k0*pmin(theta,xc1)+  k1*pmax(0,theta-xc1) + kdiff *quad.linear) # 
    }
}
#
if(is.null(m) | length(m) < 2 ) stop("Need to have two change locations see help(fitLinear2BentCable)")
    m1 <- m[1]
    m2 <- m[2]
if (is.null(axis.start)| length(axis.start) < 2) stop("Must explicitly input initial axis location as two values ")
    u <- axis.start[1]
    v <- axis.start[2]
# new graphic window if forgotten to use doSetupGraphics()
if( length(dev.list()) <1 ) dev.new()
dev.set( dev.list()[1])
 MASS::eqscplot(x, y, pch = 1)
points(u, v, pch=3, cex=2.5, col=4)
points( x[1], y[1], pch=16,col=2)
title("Outline with specified initial L2BC axis location", 
            cex = 0.9, col.main = 2, adj = 0)
#
if ( length(start.values) !=6) stop ("Must input SIX starting parameter values see help(fitLinear2BentCable)")
start.values <- c(start.values["a0"], start.values["k0"], start.values["k1"],start.values["k2"], 
                        start.values["g"], start.values["kappa"])

    a0 <- start.values["a0"]
k0 <- ifelse(angle.deg, 1/tan(start.values[2]*(pi/180)), start.values["k0"])
k1 <- ifelse(angle.deg, 1/tan(start.values[3]*(pi/180)), start.values["k1"])
k2 <- ifelse(angle.deg, 1/tan(start.values[4]*(pi/180)), start.values["k2"])
    kdiff <- k2-k1
    g <- start.values["g"] 
kappa <- ifelse( quadratic, 2,as.numeric(start.values["kappa"])  )
if(angle.deg) {
cat("\n")
cat( "Degrees in start, so as radian slope start values are ","\n")
cat( " k0   k1   k2 :    ", c(k0,k1,k2),"\n")
}  else
{
cat("\n")
cat( "Radians in start, so as degrees have start values as" ,"\n")
cat( " k0 k1 k2   : ", c((180/pi)*atan(1/c(k0,k1,k2) ) ),"\n")
}
#
#
if(quadratic)
{
    spiral.fit <- nls(~lspiral.lhs(x, y, u, v) - 
                       lspiral.rhs.q(x, y, u, v,a0,k0,k1, kdiff,g),
        trace = trace.check, control = list(tol = tolerance.step,  maxiter = 400), 
       start = list(u = u, v = v,a0=a0,k0=k0, k1 = k1,kdiff = kdiff,g=g))
    spiral.fit.coef <- coef(spiral.fit)
    u1 <- as.numeric(spiral.fit.coef[1])
    v1 <- as.numeric(spiral.fit.coef[2])
    a0 <- as.numeric(spiral.fit.coef[3])
    k0 <- as.numeric(spiral.fit.coef[4])
    k1 <- as.numeric(spiral.fit.coef[5])
    kdiff <- as.numeric(spiral.fit.coef[6])
    g <- as.numeric(spiral.fit.coef[7])
}
else
{
    spiral.fit <- nls(~lspiral.lhs(x, y, u, v) - 
                       lspiral.rhs.g(x, y, u, v,a0,k0,k1, kdiff,g,kappa),
        trace = trace.check, control = list(tol = tolerance.step,  maxiter = 400), 
       start = list(u = u, v = v,a0=a0,k0=k0, k1 = k1, kdiff = kdiff,g=g, kappa=kappa))
    spiral.fit.coef <- coef(spiral.fit)
    u1 <- as.numeric(spiral.fit.coef[1])
    v1 <- as.numeric(spiral.fit.coef[2])
    a0 <- as.numeric(spiral.fit.coef[3])
    k0 <- as.numeric(spiral.fit.coef[4])
    k1 <- as.numeric(spiral.fit.coef[5])
    kdiff <- as.numeric(spiral.fit.coef[6])
    g <- as.numeric(spiral.fit.coef[7])
    kappa <- as.numeric(spiral.fit.coef[8])

}
     fit.summary <- summary(spiral.fit)
    df1 <- fit.summary$df[2]
    sigma1 <- fit.summary$sigma
# predicted values from parameter estimates
# assume axis as fixed, calculate the radius
# and angle about this axis
#
    rp1 <- sqrt((x - u1)^2 + (y - v1)^2)
    thetap1 <- logspiral::cumulativeAngle(x, y, u1, v1)
#
# change location (radians) now based on final axis estimates
# have user fixed change locations, and are NOT estimated 
# keep the tau notation for second change to be consistent
# with bent cable forumulation
#
    change1 <- thetap1[m1]
    tau <- thetap1[m2]
    theta.d <- thetap1 - tau
a1 <- exp( log(a0) + (k0-k1)*change1  )
      k2 <- kdiff + k1
a2 <- exp(  log(a1)  - kdiff*tau ) 
    nxy <- length( x)
    parameters.df <- data.frame(a0 = a0,k0 = k0,a1 = a1, k1 = k1, 
        a2 = a2, k2 = k2, slope.diff = kdiff, tau = tau, gamma = g)
    row.names(parameters.df) <- ""
#
xr <-   NULL
theta.d <- thetap1 - tau
#
   zcode.part <-  ifelse( kappa !=2 & (theta.d + (kappa-1)*g) <= 0, 0,
                       (g*((theta.d +(kappa-1)*g)^kappa)/(kappa*g)^kappa)  )
#
   zcode <- zcode.part*(thetap1 > (tau-(kappa-1)*g) & thetap1 < tau+g ) + 
                        (theta.d)*(thetap1 > tau+g)  
#
 #
rpp1 <-  a0*exp( k0*pmin(thetap1,change1)+  k1*pmax(0,thetap1-change1) + kdiff *zcode) 
#
    deviations <- rp1 - rpp1
    sigma2 <- sqrt(var(deviations))
    n <- length(deviations)
    mssd <- (sum(diff(deviations)^2))/(2 * (n - 1))
    sigma3 <- sqrt(mssd)
    mssd.ratio <- mssd/var(deviations)
# Extend the fitted spiral to many more points from near axis
#
    start.theta <- logspiral::StartAngle(xc = x[1], yc = y[1],u1, v1)
  seq.theta <- seq( 0, max(thetap1), 0.05)
    seq.theta.d <- seq.theta - tau
    rfit <- NULL
#
   zzcode.part <-  ifelse( kappa !=2 & (seq.theta.d + (kappa-1)*g) <= 0, 0,
                        (g*((seq.theta.d +(kappa-1)*g)^kappa)/(kappa*g)^kappa)  )
#
   zzcode <- zzcode.part*(seq.theta > (tau-(kappa-1)*g) & seq.theta < tau+g ) + 
                        (seq.theta.d)*(seq.theta > tau+g)  #
#
#
rfit <- ifelse( seq.theta <= change1, a0*exp(k0*seq.theta),
               a1*exp( k1*seq.theta + kdiff*zzcode ) )
rfit.a <- ifelse( thetap1 <= change1, a0*exp(k0*thetap1),
               a1*exp( k1*thetap1 + kdiff*zcode) )
    arc.length <- sqrt(diff(x - u1)^2 + diff(y - v1)^2)
    arc.length <- cumsum(c(0, arc.length))
    xp <- rfit * cos(seq.theta)
    yp <- rfit * sin(seq.theta)
    xp.a <- rfit.a * cos(thetap1)
    yp.a <- rfit.a * sin(thetap1)
    xp1 <- xp * cos(-start.theta) + yp * sin(-start.theta)
    yp1 <- yp * cos(-start.theta) - xp * sin(-start.theta)
    xp1.a <- xp.a * cos(-start.theta) + yp.a * sin(-start.theta)
    yp1.a <- yp.a * cos(-start.theta) - xp.a * sin(-start.theta)
    xp1.a <- xp1.a + u1
    yp1.a <- yp1.a + v1
    if (flip.x) {
        xp1.a <- -1 * (xp1.a - move.by)
        u1 <- -1 * (u1 - move.by)
    }

if(angle.deg) {
k0 <- (180/pi)*atan(1/k0)
k1 <- (180/pi)*atan(1/k1)
k2 <- (180/pi)*atan(1/k2)
}
    parameters1 <- round(c(u1, v1,a0,k0, 
                 a1, k1, a2, k2, kdiff, g,kappa, tau), 4)
    parameters2 <- round(c(sigma1, sigma2, sigma3, mssd.ratio),4)
    flipped <- 0
    if (flip.x) 
        flipped <- 1
    parameters3 <- round(c(n, df1, flipped), 0)
    names(parameters1) <- c("x.axis", "y.axis","a0", "k0", 
                       "a1", "k1", "a2", "k2", "kdiff", "g","kappa", "tau")
    names(parameters2) <- c("sd.fit", "sd.deviations", "sd.msd","mssd.ratio")
    names(parameters3) <- c("N", "df", "flipped?")
    r.residuals <- deviations
    r.fitted <- rpp1
#
# locate where the bend starts & finishes on outline (angle
#  units) then find these locations in arc.length
# need to interpolate as angle is not equally spaced
# remember that tau is fixed because change location is
# given as the m th coordinate pair on the outline
#
tau1 <- tau - (kappa-1)*g
tau2 <- tau + g
tau1.loc <- which.min( abs(thetap1 - tau1) )
tau2.loc <- which.min( abs(thetap1 - tau2)  )
tau1.dist <- approx(  x = thetap1[c(tau1.loc-1, tau1.loc, tau1.loc+1)],
                      y = arc.length[c(tau1.loc-1, tau1.loc, tau1.loc+1)],
                         xout = tau1)$y
tau2.dist <- approx(  x = thetap1[c(tau2.loc-1, tau2.loc, tau2.loc+1)],
                      y = arc.length[c(tau2.loc-1, tau2.loc, tau2.loc+1)],
                         xout = tau2)$y
parameters4 <- round(c(tau1, tau1.loc, tau1.dist, tau2,tau2.loc, tau2.dist),4)
names(parameters4) <- c("tau1","coord", "arc.dist","tau2","coord","arc.dist")
#
# create output as a list of objects
#
    spiral.output <- list(parameters1, parameters2, parameters3, parameters4,
        xp1, yp1, thetap1, arc.length, deviations, r.fitted, 
        xp1.a, yp1.a)
    names(spiral.output) <- c("parameters1", "parameters2", "parameters3", 
       "parameters4", "xpredict", "ypredict", "angle", "arc.length", "deviations", 
         "fitted", "xpred.orig", "ypred.orig")
#
#

if (plot.fit) {
if( length(dev.list()) <1 ) dev.new()
if( length( dev.list()) < 3) warning("Useful for  THREE+ graphic windows open see aaSetupGraphics")
dev.set( dev.list()[2])
    x.fit <- spiral.output$xpred.orig
    y.fit <- spiral.output$ypred.orig
    xaxis <- spiral.output$parameters1[1]
    yaxis <- spiral.output$parameters1[2]
    requireNamespace("MASS")
    MASS::eqscplot(x, y, pch = 1, axes = F, xlab = "", ylab = "")
    lines(x.fit, y.fit, col = 2, lwd = 2)
points( x.fit[m1], y.fit[m1], pch=16, col=4, cex=1.4)
points( x.fit[m2], y.fit[m2], pch=16, col=4, cex=1.4)
points(x.fit[tau1.loc], y.fit[tau1.loc], pch=15, col=4, cex=1.5)
points(x.fit[tau2.loc], y.fit[tau2.loc], pch=15, col=4, cex=1.5)
    points(xaxis, yaxis, pch = 10, cex = 2.5, col = 2)
}
if (plot.deviations) {
if( length(dev.list()) <1 ) dev.new()
if( length( dev.list()) < 3) warning("Useful for  THREE+ graphic windows open see aaSetupGraphics")
dev.set( dev.list()[3])
    deviations <- spiral.output$deviations
    arc.length <- spiral.output$arc.length
    plot(arc.length, deviations, type = "s", xlab = "Secant or Arc length from umbo (mm)", 
        ylab = "Spiral deviations(mm)")
    abline(h = 0, col = 4)
    abline(v = arc.length[c(m1,m2)], col = 2, lwd = 2, lty = 1)
abline( v=c(tau1.dist, tau2.dist), col=2, lwd=2, lty=2)
    axis(4, labels = F)
    axis(3, labels = F)
    mtext(paste("Spiral deviations : ", title.label), side = 3, 
        outer = T, line = -2, cex = 1.2, col = 4, adj = 0.05)
}

if (plot.diagnostics==FALSE)
{
#
# check for extreme deviations around change location (+/- 4 coords and 95% quantile)
#
dev95 <- quantile( abs(deviations), prob=0.95)
if( max(deviations[(m1-4):(m1+4)] ) > dev95) warning("Extreme deviation(s) near 1st change location. Check spiral shape",call. = FALSE)
if( max(deviations[(m2-4):(m2+4)] ) > dev95) warning("Extreme deviation(s) near 2nd change location. Check spiral shape",call. = FALSE)
}
#

if (plot.diagnostics) {
# includes a check for extreme deviations
if( length(dev.list()) <1 ) dev.new()
if( length( dev.list()) < 4) warning("Useful for  THREE+ graphic windows open see aaSetupGraphics")
dev.set( dev.list()[4])
  deviations <- spiral.output$deviations
plotDiagnostics(spiral.output=spiral.output, change=c(m1, m2, NULL))}
    return(spiral.output)
}
