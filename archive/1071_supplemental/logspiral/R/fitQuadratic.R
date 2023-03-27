fitQuadratic <-
function (x, y, axis.start = c(NULL, NULL), flip.x = FALSE, dist.zero = 2, 
    spiral.angle = 40, quadratic = 0.02, 
     move.by = NULL, tolerance.step = 0.01, angle.deg = TRUE,
     plot.fit=FALSE, plot.deviations=FALSE, plot.diagnostics=FALSE,
title.label="Quadratic spiral fit to outline") 
{
    lspiral.lhs <- function(x, y, u, v) sqrt((x - u)^2 + (y - 
        v)^2)
    lspiral.rhs <- function(x, y, u, v, a1, k1, q1) {
        theta <- cumulativeAngle(x, y, u, v)
        a1 * exp(k1 * theta + q1 * theta^2)
    }
    if (is.null(axis.start)) {
if( length(dev.list()) <1 ) dev.new()
dev.set( dev.list()[1])
        MASS::eqscplot(x, y, pch = 1)
        title("Please mouse (left) click your best for Q axis location", 
            cex = 0.9, col.main = 2, adj = 0)
        points(x[1], y[1], pch = 16, col = 2, cex = 1.1)
        locate.xy <- locator(n = 1)
        start.u <- locate.xy$x
        start.v <- locate.xy$y
points(start.u, start.v, pch=3, cex=2.5, col=4)
    }
    else {
        start.u <- axis.start[1]
        start.v <- axis.start[2]
if( length(dev.list()) <1 ) dev.new()
dev.set( dev.list()[1])
 MASS::eqscplot(x, y, pch = 1)
points(start.u, start.v, pch=3, cex=2.5, col=4)
title("Outline with specified initial Q axis location", 
            cex = 0.9, col.main = 2, adj = 0)
    }
    u <- start.u
    v <- start.v
    a1.start <- dist.zero
    k1.start <- 1/(tan(spiral.angle * (pi/180)))
    q.start <- quadratic
    spiral.fit <- nls(~lspiral.lhs(x, y, u, v) - lspiral.rhs(x, 
        y, u, v, a1, k1, q), trace = F, control = list(warnOnly = TRUE, 
        tol = tolerance.step, maxiter = 400), start = list(u = u, 
        v = v, a1 = a1.start, k1 = k1.start, q = q.start))
    spiral.fit.coef <- coef(spiral.fit)
    u1 <- as.numeric(spiral.fit.coef[1])
    v1 <- as.numeric(spiral.fit.coef[2])
    a1 <- as.numeric(spiral.fit.coef[3])
    k1 <- as.numeric(spiral.fit.coef[4])
    q1 <- as.numeric(spiral.fit.coef[5])
    fit.summary <- summary(spiral.fit)
    df1 <- fit.summary$df[2]
    sigma1 <- fit.summary$sigma
    rp1 <- sqrt((x - u1)^2 + (y - v1)^2)
    thetap1 <- cumulativeAngle(x, y, u1, v1)
    rpp1 <- a1 * exp(k1 * thetap1 + q1 * thetap1^2)
    deviations <- rp1 - rpp1
    sigma2 <- sqrt(var(deviations))
    n <- length(deviations)
    mssd <- (sum(diff(deviations)^2))/(2 * (n - 1))
    sigma3 <- sqrt(mssd)
    mssd.ratio <- mssd/var(deviations)
    v.deviations <- deviations * sin(atan(1/k1))
    start.theta <- StartAngle(xc = x[1], yc = y[1], 
        u1, v1)
    seq.theta <- seq(-(17/2) * pi, max(thetap1) + 0.1, 0.05)
    r.fit <- a1 * exp(k1 * seq.theta + q1 * seq.theta^2)
    rfit.a <- a1 * exp(k1 * thetap1 + q1 * thetap1^2)
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
    if (angle.deg) 
        k1 <- (180/pi) * atan(1/k1)
    parameters1 <- round(c(u1, v1, a1, k1, q1), 4)
    parameters2 <- round(c(sigma1, sigma2, sigma3, mssd.ratio), 
        4)
    flipped <- 0
    if (flip.x) 
        flipped <- 1
    parameters3 <- round(c(n, df1, flipped), 0)
    names(parameters1) <- c("x.axis", "y.axis", "a1", "k1", "q")
    names(parameters2) <- c("sd.fit", "sd.deviations", "sd.msd", 
        "mssd.ratio")
    names(parameters3) <- c("N", "df", "flip?")
    r.residuals <- deviations
    r.fitted <- rfit.a
    spiral.output <- list(parameters1, parameters2, parameters3, 
        xp1, yp1, thetap1, arc.length, deviations, r.fitted, 
        xp1.a, yp1.a)
    names(spiral.output) <- c("parameters1", "parameters2", "parameters3", 
        "xpredict", "ypredict", "angle", "arc.length", "deviations", 
        "fitted", "xpred.orig", "ypred.orig")


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
     axis(4, labels = F)
    axis(3, labels = F)
    mtext(paste("Spiral deviations : ", title.label), side = 3, 
        outer = T, line = -2, cex = 1.2, col = 4, adj = 0.05)
}
if (plot.diagnostics) {
if( length(dev.list()) <1 ) dev.new()
if( length( dev.list()) < 4) warning("Useful for  THREE+ graphic windows open see aaSetupGraphics")
dev.set( dev.list()[4])
  deviations <- spiral.output$deviations
plotDiagnostics(spiral.output=spiral.output, change=c(NULL, NULL, NULL))
}

#
    return(spiral.output)
}
