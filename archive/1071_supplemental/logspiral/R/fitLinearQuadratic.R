fitLinearQuadratic <-
function (x, y, m = NULL, axis.start = c(NULL,NULL),
start.values=c(a1=2,k1=40, k2=55), angle.deg=TRUE,  
plot.fit=FALSE, plot.deviations=FALSE, plot.diagnostics=FALSE,
flip.x = FALSE, move.by = NULL, tolerance.step = 0.1, trace.check=FALSE,
title.label = " ") 
{
    lspiral.lhs <- function(x, y, u, v) sqrt((x - u)^2 + (y - v)^2)
if( is.null(m) ) stop( "Must supply the coordinate location of the transition (m)")
    m1 <- m
    lspiral.rhs <- function(x, y, u, v, a1, k1, k2, m = m1) {
        theta <- logspiral::cumulativeAngle(x, y, u, v)
        xc <- theta[m]
        a1 * exp(ifelse(theta <= xc, k1 * theta, xc * (k1 - k2)/2 + 
            k2 * theta + (k1 - k2) * theta^2/(2 * xc)))
    }
 if (is.null(axis.start)) {
  if( length(dev.list()) <1 ) dev.new()
dev.set( dev.list()[1])
        MASS::eqscplot(x, y, pch = 1)
        title("Please mouse (left) click your best guess at axis location", 
            cex = 0.9, col.main = 2, adj = 0)
        points(x[1], y[1], pch = 16, col = 2, cex = 1.1)
        locate.xy <- locator(n = 1)
        u <- locate.xy$x
        v <- locate.xy$y
points (u,v, pch=3, cex=2.5, col=4)
    }
    else {
if (length(axis.start) < 2) stop("Must input initial axis location as TWO values ")
        u <- axis.start[1]
        v <- axis.start[2]
if( length(dev.list()) <1 ) dev.new()
dev.set( dev.list()[1])
 MASS::eqscplot(x, y, pch = 1)
points(u, v, pch=3, cex=2.5, col=4)
points( x[1], y[1], pch=16,col=2)
title("Outline with specified initial LQ axis location", 
            cex = 0.9, col.main = 2, adj = 0)
    }
#
if( length(start.values)!=3 ) stop("Must supply THREE starting parameter values, see help(fitLinearQuadratic)")
start.values <- c( start.values["a1"], start.values["k1"], start.values["k2"])
a1 <- as.numeric(start.values["a1"])
k1 <- ifelse( angle.deg, 1/tan(start.values[2]*(pi/180)), as.numeric(start.values["k1"]))
k2 <- ifelse( angle.deg, 1/tan(start.values[3]*(pi/180)), as.numeric(start.values["k2"]))

    spiral.fit <- nls(~lspiral.lhs(x, y, u, v) - lspiral.rhs(x, 
        y, u, v, a1, k1, k2), trace = trace.check, control = list(tol = tolerance.step, 
        maxiter = 400), start = list(u = u, v = v, a1 = a1, k1 = k1, k2 = k2))
    spiral.fit.coef <- coef(spiral.fit)
    u1 <- as.numeric(spiral.fit.coef[1])
    v1 <- as.numeric(spiral.fit.coef[2])
    a1 <- as.numeric(spiral.fit.coef[3])
    k1 <- as.numeric(spiral.fit.coef[4])
    k2 <- as.numeric(spiral.fit.coef[5])
    fit.summary <- summary(spiral.fit)
    df1 <- fit.summary$df[2]
    sigma1 <- fit.summary$sigma
    rp1 <- sqrt((x - u1)^2 + (y - v1)^2)
    thetap1 <- logspiral::cumulativeAngle(x, y, u1, v1)
    zz <- thetap1[m1]
    theta.change <- zz
    q <- (k1 - k2)/(2 * zz)
    log.a2 <- log(a1) + k1 * zz - k2 * zz - q * zz^2
    a2 <- exp(log.a2)
    rpp1 <- ifelse(thetap1 <= zz, log(a1) + k1 * thetap1, log(a2) + 
        k2 * thetap1 + q * (thetap1^2))
    rpp1 <- exp(rpp1)
    deviations <- rp1 - rpp1
    sigma2 <- sqrt(var(deviations))
    n <- length(deviations)
    mssd <- (sum(diff(deviations)^2))/(2 * (n - 1))
    sigma3 <- sqrt(mssd)
    mssd.ratio <- mssd/var(deviations)
    start.theta <- logspiral::StartAngle(xc = x[1], yc = y[1], 
        u1, v1)
    seq.theta <- seq(-(17/2) * pi, max(thetap1) + 0.1, 0.05)
    r.fit <- ifelse(seq.theta <= zz, log(a1) + k1 * seq.theta, 
        log(a2) + k2 * seq.theta + q * (seq.theta^2))
    r.fit <- exp(r.fit)
    rfit.a <- ifelse(thetap1 <= zz, log(a1) + k1 * thetap1, log(a2) + 
        k2 * thetap1 + q * (thetap1^2))
    rfit.a <- exp(rfit.a)
    arc.length <- sqrt(diff(x - u1)^2 + diff(y - v1)^2)
    arc.length <- cumsum(c(0, arc.length))
    xp <- r.fit * cos(seq.theta)
    yp <- r.fit * sin(seq.theta)
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
k1 <- (180/pi)*atan(1/k1)
k2 <- (180/pi)*atan(1/k2)
}
    parameters1 <- round(c(u1, v1, a1, k1, a2, k2, q), 4)
    parameters2 <- round(c(sigma1, sigma2, sigma3, mssd.ratio), 
        4)
    flipped <- 0
    if (flip.x) 
        flipped <- 1
    parameters3 <- round(c(n, df1, flipped), 0)
    names(parameters1) <- c("x.axis", "y.axis", "a1", "k1", "a2", 
        "k2", "q")
    names(parameters2) <- c("sd.fit", "sd.deviations", "sd.msd", 
        "mssd.ratio")
    names(parameters3) <- c("N", "df", "flipped?")
    r.residuals <- deviations
    r.fitted <- rpp1
    spiral.output <- list(parameters1, parameters2, parameters3, 
        xp1, yp1, thetap1, arc.length, deviations, r.fitted, 
        xp1.a, yp1.a)
    names(spiral.output) <- c("parameters1", "parameters2", "parameters3", 
        "xpredict", "ypredict", "angle", "arc.length", "deviations", 
        "fitted", "xpred.orig", "ypred.orig")
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
    abline(v = arc.length[m1], col = 2, lwd = 2, lty = 1)
    axis(4, labels = F)
    axis(3, labels = F)
    mtext(paste("Spiral deviations : "), side = 3, 
        outer = T, line = -2, cex = 1.2, col = 4, adj = 0.05)
}
if(plot.diagnostics==FALSE)
{
#
# check for extreme deviations around change location (+/- 4 coords and 95% quantile)
#
dev95 <- quantile( abs(deviations), prob=0.95)
if( max(deviations[(m1-4):(m1+4)] ) > dev95) warning("Extreme deviation(s) near change location. Check spiral shape",call. = FALSE)
}
#

if (plot.diagnostics) {
if( length(dev.list()) <1 ) dev.new()
if( length( dev.list()) < 4) warning("Useful for  THREE+ graphic windows open see aaSetupGraphics")
dev.set( dev.list()[4])
  deviations <- spiral.output$deviations
plotDiagnostics(spiral.output=spiral.output, change=c(m1, NULL, NULL))
}
    return(spiral.output)
}
