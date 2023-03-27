generateCurvedSpiral <-
function (nstart = 200, dist.zero = 2, k = 50, quadratic = 0.03, 
    cubic = 0.03, quartic = 0.01, noise = 1e-05, theta.start = 1e-04, 
    theta.end = 1.25, equal = FALSE, equal.n = 200, move = TRUE, 
    move.factor = 1000, plot.it = TRUE, print.summary = TRUE, 
    show.change = TRUE) 
{
    a <- dist.zero
    k <- 1/(tan(k * (pi/180)))
    q <- quadratic
    c <- cubic
    qq <- quartic
    xtheta <- seq(theta.start, theta.end * pi, length = 200)
    requireNamespace("logspiral")
    xxtheta <- logspiral::equalSpace(xtheta, exp(xtheta), nequal = nstart)
    theta <- xxtheta$x
    xr <- a * exp(k * theta + q * theta^2 + c * theta^3 + qq * 
        theta^4)
    x <- xr * cos(theta)
    y <- xr * sin(theta)
    axis.locate <- c(0, 0)
    if (equal) {
        x.df <- logspiral::equalSpace(x, y, nequal = equal.n, 
            light.smooth = TRUE)
        xx <- x.df$x
        yy <- x.df$y
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
        xxr[1] <- a
    }
    xxtheta <- ifelse(xxtheta < 0, xxtheta + 2 * pi, xxtheta)
    if (move) {
        xy.move <- abs(move.factor * floor(min(xx)/move.factor))
        xx <- xx + xy.move
        yy <- yy + xy.move
        axis.locate <- axis.locate + xy.move
    }
    arc.length <- sqrt(diff(xx)^2 + diff(yy)^2)
    arc.diff <- arc.length
    arc.length <- c(0, cumsum(arc.length))
    arc.avg <- median(arc.diff)
    arc.sd <- sqrt(var(arc.diff))
    arc <- max(arc.length)
    if (print.summary) {
        cat("Parameters: a = ", round(a, 4), "  k =", round(k, 
            4), "  q=", round(q, 4), "c=", round(c, 4), "qq=", 
            round(qq, 4), "\n")
        cat("Axis location (x,y) : ", axis.locate)
        cat("  Noise: ", noise, "\n")
        cat("ARC: avg incr., sd incr., total length:  ", round(c(arc.avg, 
            arc.sd, arc), 4), "\n")
    }
    if (plot.it) {
        ngraphics <- length(dev.list())
        if (ngraphics < 4) 
            warning("Less than four graphic windows, consider using asetup.graphics()")
        requireNamespace("MASS")
        dev.set(3)
        MASS::eqscplot(xx, yy, pch = 16, cex = 0.5, col = 2, 
            xlab = "", ylab = "")
        points(axis.locate[1], axis.locate[2], pch = 10, cex = 2, 
            col = 4)
        axis(3, labels = F)
        axis(4, labels = F)
    }
    xy <- data.frame(x = xx, y = yy, logr = log(xxr), angle = xxtheta, 
        arc.length = arc.length)
    xy
}
