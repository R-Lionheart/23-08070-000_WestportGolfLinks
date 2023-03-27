generateQuadraticLinearSpiral <-
function (nstart = 200, change = 100, k1 = 40, k2 = 50, q = 0, 
    dist.zero = 2, noise = 0.001, theta.start = 1e-04, theta.end = 1.25, 
    equal = FALSE, equal.n = 200, move = TRUE, move.factor = 1000, 
    plot.it = TRUE, show.change = TRUE) 
{
    a1 <- dist.zero
   cat("  Change at coordinate pair :", change,"Spiral angles : ",c(k1,k2), "\n")

    k1 <- 1/(tan(k1 * (pi/180)))
    if (k2 != 0) 
        k2 <- 1/(tan(k2 * (pi/180)))
    xtheta <- seq(theta.start, theta.end * pi, length = 200)
    requireNamespace("logspiral")
    xxtheta <- logspiral::equalSpace(xtheta, exp(xtheta), nequal = nstart)
    theta <- xxtheta$x
    xc <- theta[change]
    if (k2 != 0 & q != 0) 
        stop("must start with either k2 or q as zero, try q=0 (the default)")
    if (k2 != 0 & q == 0) 
        q <- (k2 - k1)/(2 * xc)
    if (k2 == 0 & q != 0) 
        k2 <- k1 + 2 * xc * q
    a2 <- exp(log(a1) + k1 * xc - k2 * xc + q * xc^2)
    cat("Parameters: a1", round(a1, 4), " k1", round(k1, 4), 
        " q", round(q, 4), " a2", round(a2, 4), " k2", round(k2, 
            4), "\n")
    xr <- ifelse(theta <= xc, log(a1) + k1 * theta + q * (theta^2), 
        log(a2) + k2 * theta)
    xr <- exp(xr)
    x <- xr * cos(theta)
    y <- xr * sin(theta)
    axis.locate <- c(0, 0)
    if (equal) {
        x.df <- logspiral::equalSpace(x, y, nequal = equal.n, 
            light.smooth = TRUE)
        xx <- x.df$x
        yy <- x.df$y
        change <- round((change * nstart)/equal.n, 0)
    }
    else {
        xx <- x
        yy <- y
    }
    xn <- length(xx)
    xx <- xx + rnorm(xn, 0, noise)
    yy <- yy + rnorm(xn, 0, noise)
    xxr <- sqrt(xx^2 + yy^2)
    xxtheta <- atan2(yy, xx)
    if (xxtheta[1] < 0) {
        xxtheta[1] <- 0
        xxr[1] <- a1
    }
    xxtheta <- ifelse(xxtheta < 0, xxtheta + 2 * pi, xxtheta)
    if (move) {
        xy.move <- abs(move.factor * floor(min(xx)/move.factor))
        xx <- xx + xy.move
        yy <- yy + xy.move
        axis.locate <- axis.locate + xy.move
    }
    cat("Axis location (x,y) :", axis.locate, "\n")
    arc.length <- sqrt(diff(xx)^2 + diff(yy)^2)
    arc.diff <- arc.length
    arc.length <- c(0, cumsum(arc.length))
    arc.avg <- median(arc.diff)
    arc.sd <- sqrt(var(arc.diff))
    arc <- max(arc.length)
    cat("ARC: avg incr., sd incr., total length:  ", round(c(arc.avg, 
        arc.sd, arc), 4), "\n")
    if (plot.it) {
        requireNamespace("MASS")
        dev.set(3)
        MASS::eqscplot(xx, yy, pch = 16, cex = 0.5, col = 2, 
            xlab = "", ylab = "")
        points(axis.locate[1], axis.locate[2], pch = 10, cex = 2, 
            col = 4)
        axis(3, labels = F)
        axis(4, labels = F)
    }
    if (plot.it && show.change) 
        points(xx[change], yy[change], cex = 0.7, col = 4, pch = 15)
    xy <- data.frame(x = xx, y = yy, logr = log(xxr), angle = xxtheta, 
        arc.length = arc.length)
    xy
}
