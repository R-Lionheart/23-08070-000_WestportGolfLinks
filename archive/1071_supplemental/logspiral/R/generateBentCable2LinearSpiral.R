generateBentCable2LinearSpiral <-
function (nstart = 200, change = c(50, 150), k1 = 35, k2 = 40, 
    k3 = 55, g = 0.3, kappa = 2, dist.zero = 2, noise = 1e-04, 
    theta.start = 1e-04, theta.end = 1.25, equal = FALSE, equal.n = 200, 
    move = TRUE, move.factor = 1000, plot.it = TRUE, include.title = TRUE, 
    show.change = TRUE, print.summary = TRUE) 
{
    cat("Input spiral angles k1,k2,k3 (degrees) : ", c(k1, k2, 
        k3), "\n")
    a1 <- dist.zero
    k1 <- 1/(tan(k1 * (pi/180)))
    k2 <- 1/(tan(k2 * (pi/180)))
    k3 <- 1/(tan(k3 * (pi/180)))
    theta <- seq(theta.start, theta.end * pi, length = 200)
    kdiff <- k2 - k1
    requireNamespace("logspiral")
    xxtheta <- logspiral::equalSpace(theta, exp(theta), nequal = nstart)
    theta <- xxtheta$x
    change1 <- change[1]
    tau <- theta[change1]
    change2 <- change[2]
    theta2 <- theta[change2]
   #
    a2 <- exp(log(a1) - kdiff * tau)
    a3 <- exp( log(a1) + (k1-k2)*tau + (k2-k3)*theta2 ) 
    parameters.df <- data.frame(a1 = a1, k1 = k1, a2 = a2, k2 = k2, 
        a3 = a3, k3 = k3, slope.diff = kdiff, tau = tau, gamma = g, 
        kappa = kappa)
    row.names(parameters.df) <- ""
    xr <- NULL
    theta.first <- theta[1:change2]
    theta.second <- theta[(change2 + 1):nstart]
    theta.d <- theta - tau
#
    zzgen.part <- ifelse(kappa != 2 & (theta - tau + (kappa - 
        1) * g) <= 0, 0, (g * ((theta - tau + (kappa - 1) * g)^kappa)/(kappa * 
        g)^kappa))
    zzgen <- zzgen.part * (theta > (tau - (kappa - 1) * g) & 
        theta < tau + g) + (theta - tau) * (theta > tau + g)
    xr <- ifelse(theta <= theta2,  log(a1) +  k1 * theta + kdiff * zzgen, log(a3) + k3 * theta )
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
        xxtheta <- xxtheta - xxtheta[1]
        xxr[1] <- a1
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
    if (plot.it) {
        requireNamespace("MASS")
        dev.set(3)
        MASS::eqscplot(xx, yy, pch = 16, cex = 0.5, col = 2, 
            xlab = "", ylab = "")
        points(axis.locate[1], axis.locate[2], pch = 10, cex = 2, 
            col = 4)
        axis(3, labels = F)
        axis(4, labels = F)
        if (include.title) {
            in.ang <- round((180/pi) * atan(1/c(k1, k2, k3)), 
                1)
            title(paste("General Bent Cable 2 Linear, spiral angles: ", 
                in.ang[1], in.ang[2], in.ang[3]), adj = 0, cex.main = 1, 
                col.main = 4)
            mtext(paste("Gamma = ", g, "   Kappa = ", kappa, 
                "  a1 = ", a1), side = 1, adj = 0, col = 4, line = 2.5)
        }
    }
    if (print.summary) {
        print(parameters.df)
        cat("Axis location (x,y) :", axis.locate)
        cat("   Change coordinates at :", change, "\n")
        cat("ARC: avg incr., sd incr., total length:  ", round(c(arc.avg, 
            arc.sd, arc), 4), "\n")
    }
    if (plot.it && show.change) 
        points(xx[change], yy[change], cex = 0.7, col = 4, pch = 15)
    xy <- data.frame(x = xx, y = yy, logr = log(xxr), angle = xxtheta, 
        arc.length = arc.length)
    xy
}
