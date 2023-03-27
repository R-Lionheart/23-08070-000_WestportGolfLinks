fitL4Spiral <-
function (x, y, m = c(NULL, NULL, NULL), start.u = NULL, start.v = NULL, 
  start.values=c(a=2, k1=0.2, k2=0.2, k3=0.2, k4=0.2),
    flip.x = FALSE, move.by = NULL, tolerance.step = 0.01, angle.deg=TRUE) 
{
    lspiral.lhs <- function(x, y, u, v) sqrt((x - u)^2 + (y - v)^2)
    m1 <- m[1]
    m2 <- m[2]
    m3 <- m[3]
    lspiral.rhs <- function(x, y, u, v, a, k1, k2, k3, k4, mc1 = m1, 
        mc2 = m2, mc3 = m3) {
        xtheta <- cumulativeAngle(x, y, u, v)
        xc1 <- xtheta[mc1]
        xc2 <- xtheta[mc2]
        xc3 <- xtheta[mc3]
        a * exp(k1 * pmin(xtheta, xc1)) * exp(k2 * pmin(xc2 - 
            xc1, pmax(0, xtheta - xc1))) * exp(k3 * pmin(xc3 - 
            xc2, pmax(0, xtheta - xc2))) * exp(k4 * pmax(0, xtheta - 
            xc3))
    }
    u <- start.u
    v <- start.v
if ( length(start.values) !=5) stop ("Must input FIVE starting parameter values see help(fitL4Spiral)")
a <- start.values["a"]
k1 <- start.values["k1"]
k2 <- start.values["k2"]
k3 <- start.values["k3"]
k4 <- start.values["k4"]
#
    spiral.fit <- nls(~lspiral.lhs(x, y, u, v) - lspiral.rhs(x, 
        y, u, v, a, k1, k2, k3, k4), trace = F, control = list(tol = tolerance.step, 
        maxiter = 400), start = list(u = u, v = v, a = a, k1 = k1, 
        k2 = k2, k3 = k3, k4 = k4))
    spiral.fit.coef <- coef(spiral.fit)
    u1 <- as.numeric(spiral.fit.coef[1])
    v1 <- as.numeric(spiral.fit.coef[2])
    a1 <- as.numeric(spiral.fit.coef[3])
    k1 <- as.numeric(spiral.fit.coef[4])
    k2 <- as.numeric(spiral.fit.coef[5])
    k3 <- as.numeric(spiral.fit.coef[6])
    k4 <- as.numeric(spiral.fit.coef[7])
    fit.summary <- summary(spiral.fit)
    df1 <- fit.summary$df[2]
    sigma1 <- fit.summary$sigma
    rp1 <- sqrt((x - u1)^2 + (y - v1)^2)
    thetap1 <- cumulativeAngle(x, y, u1, v1)
    zz <- thetap1[m1]
    zz1 <- thetap1[m1]
    zz2 <- thetap1[m2]
    zz3 <- thetap1[m3]
    a2 <- a1 * exp((k1 - k2) * zz1)
    a3 <- a2 * exp((k2 - k3) * zz2)
    a4 <- a3 * exp((k3 - k4) * zz3)
    theta.change <- c(zz1, zz2, zz3)
    rpp1 <- ifelse(thetap1 < zz1, a1 * exp(k1 * thetap1), ifelse(thetap1 < 
        zz2, a2 * exp(k2 * thetap1), ifelse(thetap1 < zz3, a3 * 
        exp(k3 * thetap1), a4 * exp(k4 * thetap1))))
    deviations <- rp1 - rpp1
    sigma2 <- sqrt(var(deviations))
    n <- length(deviations)
    mssd <- (sum(diff(deviations)^2))/(2 * (n - 1))
    sigma3 <- sqrt(mssd)
    mssd.ratio <- mssd/var(deviations)
    v.deviations <- ifelse(thetap1 < zz1, deviations * sin(atan(1/k1)), 
        ifelse(thetap1 < zz2, deviations * sin(atan(1/k2)), ifelse(thetap1 < 
            zz3, deviations * sin(atan(1/k3)), deviations * sin(atan(1/k4)))))
    sigma4 <- sqrt(var(v.deviations))
    mssd.v <- (sum(diff(v.deviations)^2))/(2 * (n - 1))
    sigma5 <- sqrt(mssd.v)
    start.theta <- StartAngle(xc = x[1], yc = y[1], u1, v1)
    seq.theta <- seq(-(17/2) * pi, max(thetap1) + 0.1, 0.05)
    r.fit <- ifelse(seq.theta < zz1, a1 * exp(k1 * seq.theta), 
        ifelse(seq.theta < zz2, a2 * exp(k2 * seq.theta), ifelse(seq.theta < 
            zz3, a3 * exp(k3 * seq.theta), a4 * exp(k4 * seq.theta))))
    rfit.a <- ifelse(thetap1 < zz1, a1 * exp(k1 * thetap1), ifelse(thetap1 < 
        zz2, a2 * exp(k2 * thetap1), ifelse(thetap1 < zz3, a3 * 
        exp(k3 * thetap1), a4 * exp(k4 * thetap1))))
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
k3 <- (180/pi)*atan(1/k3)
k4 <- (180/pi)*atan(1/k4)
}

    parameters1 <- round(c(u1, v1, a1, a2, a3, a4, k1, k2, k3, 
        k4), 4)
    parameters2 <- round(c(sigma1, sigma2, sigma3, mssd.ratio, 
        sigma4, sigma5), 4)
    flipped <- 0
    if (flip.x) 
        flipped <- 1
    parameters3 <- round(c(n, df1, flipped), 0)
    names(parameters1) <- c("x.axis", "y.axis", "a1", "a2", "a3", 
        "a4", "k1", "k2", "k3", "k4")
    names(parameters2) <- c("sd.fit", "sd.deviations", "sd.msd", 
        "mssd.ratio", "sd.vdev", "sd.vmsd")
    names(parameters3) <- c("N", "df", "flipped?")
    r.residuals <- deviations
    r.fitted <- rpp1
    spiral.output <- list(parameters1, parameters2, parameters3, 
        xp1, yp1, thetap1, arc.length, deviations, v.deviations, 
        r.fitted, xp1.a, yp1.a)
    names(spiral.output) <- c("parameters1", "parameters2", "parameters3", 
        "xpredict", "ypredict", "angle", "arc.length", "deviations", 
        "v.deviations", "fitted", "xpred.orig", "ypred.orig")
    return(spiral.output)
}
