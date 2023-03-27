fitL1Spiral <-
function (x, y, start.u = NULL, start.v = NULL, 
start.values =c( a=2,k=0.2), flip.x = FALSE, 
    move.by = NULL, tolerance.step = 0.01,angle.deg=TRUE) 
{
    lspiral.lhs <- function(x, y, u, v) sqrt((x - u)^2 + (y - 
        v)^2)
    lspiral.rhs <- function(x, y, u, v, a, k) {
        xtheta <- cumulativeAngle(x, y, u, v)
        a * exp(k * xtheta)
    }
    u <- start.u
    v <- start.v
if ( length(start.values) !=2) stop ("Must input TWO starting parameter values see help(fitL2Spiral)")
a <- start.values["a"]
k <- start.values["k"]
    spiral.fit <- nls(~lspiral.lhs(x, y, u, v) - lspiral.rhs(x, 
        y, u, v, a, k), trace = F, control = list(tol = tolerance.step, 
        maxiter = 400), start = list(u = u, v = v, a = a, k = k))
    spiral.fit.coef <- coef(spiral.fit)
    u1 <- as.numeric(spiral.fit.coef[1])
    v1 <- as.numeric(spiral.fit.coef[2])
    a1 <- as.numeric(spiral.fit.coef[3])
    k1 <- as.numeric(spiral.fit.coef[4])
    fit.summary <- summary(spiral.fit)
    df1 <- fit.summary$df[2]
    sigma1 <- fit.summary$sigma
    rp1 <- sqrt((x - u1)^2 + (y - v1)^2)
    thetap1 <- cumulativeAngle(x, y, u1, v1)
    rpp1 <- a1 * exp(k1 * thetap1)
    deviations <- rp1 - rpp1
    sigma2 <- sqrt(var(deviations))
    n <- length(deviations)
    mssd <- (sum(diff(deviations)^2))/(2 * (n - 1))
    sigma3 <- sqrt(mssd)
    mssd.ratio <- mssd/var(deviations)
    v.deviations <- deviations * sin(atan(1/k1))
    sigma4 <- sqrt(var(v.deviations))
    mssd.v <- (sum(diff(v.deviations)^2))/(2 * (n - 1))
    sigma5 <- sqrt(mssd.v)
    start.theta <- StartAngle(xc = x[1], yc = y[1], u1, v1)
    seq.theta <- seq(-(17/2) * pi, max(thetap1) + 0.1, 0.05)
    r.fit <- a1 * exp(k1 * seq.theta)
    rfit.a <- a1 * exp(k1 * thetap1)
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
if(angle.deg) k1 <- (180/pi)*atan(1/k1)
#
    parameters1 <- round(c(u1, v1, a1, k1), 4)
    parameters2 <- round(c(sigma1, sigma2, sigma3, mssd.ratio, 
        sigma4, sigma5), 4)
    flipped <- 0
    if (flip.x) 
        flipped <- 1
    parameters3 <- round(c(n, df1, flipped), 0)
    names(parameters1) <- c("x.axis", "y.axis", "a1", "k1")
    names(parameters2) <- c("sd.fit", "sd.deviations", "sd.msd", 
        "mssd.ratio", "sd.vdev", "sd.vmsd")
    names(parameters3) <- c("N", "df", "flip?")
    r.residuals <- deviations
    r.fitted <- rfit.a
    spiral.output <- list(parameters1, parameters2, parameters3, 
        xp1, yp1, thetap1, arc.length, deviations, v.deviations, 
        r.fitted, xp1.a, yp1.a)
    names(spiral.output) <- c("parameters1", "parameters2", "parameters3", 
        "xpredict", "ypredict", "angle", "arc.length", "deviations", 
        "v.deviations", "fitted", "xpred.orig", "ypred.orig")
    return(spiral.output)
}
