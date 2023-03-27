arcSmooth2D <-
function (x = x, y = y, smooth.factor = NULL, summary = FALSE, 
    plot.deviation = FALSE, plot.it = FALSE, print.summary = FALSE, 
    output = TRUE) 
{
    arc <- sqrt((diff(x)^2 + diff(y)^2))
    arc.length <- cumsum(c(0, arc))
    if (is.null(smooth.factor)) 
        smooth.factor <- bestSmooth(x, y, plot.it = FALSE)
    xsmooth <- smooth.spline(arc.length, x, spar = smooth.factor)
    ysmooth <- smooth.spline(arc.length, y, spar = smooth.factor)
    xp <- xsmooth$y
    yp <- ysmooth$y
#
# distance from outline to smooth to a using formula in Maxwell (1958) page 81
A <- diff(y)
B <- -1*diff(x)
C <- diff(x)*y[-1] -1*diff(y)*x[-1]
#
dist.pts <- (A*xp[-1] + B*yp[-1] + C)/sqrt( A^2 + B^2)
dist.start <- (A[1]*xp[1] + B[1]*yp[1] + C[1])/sqrt( A[1]^2 + B[1]^2)
dist.pts <- c(dist.start, dist.pts)
zdeviation <- -1*dist.pts
zn <- length(zdeviation)
#
    dev.mad <- mad(zdeviation)
    if (plot.deviation) {
        dev.set(4)
        plot(arc.length, zdeviation - mean(zdeviation), type = "s", 
            xlab = " Outline arc length from start", ylab = "Centred deviation", 
            col = 4)
        abline(h = 0, col = 2)
        axis(4, labels = F)
        axis(3, labels = F)
    }
    if (plot.it) {
        dev.set(5)
        requireNamespace("MASS")
        MASS::eqscplot(x, y, xlab = "", ylab = "")
        lines(xp, yp, col = 2, lwd = 1.5)
        axis(3, labels = F)
        axis(4, labels = F)
    }
    zmssd <- (sum(diff(zdeviation)^2))/(2 * (zn - 1))
    zmssd.ratio <- zmssd/var(zdeviation)
    dev.std <- sqrt(var(zdeviation))
    zmssd.ratio <- round(zmssd.ratio, 4)
    dev.std <- round(dev.std, 4)
    dev.mad <- round(dev.mad, 4)
    if (summary) 
        return(c(smooth.factor, zmssd.ratio, dev.std, dev.mad))
    if (print.summary) 
        cat(c("smooth factor =", smooth.factor, "mssd.ratio =", 
            zmssd.ratio, "usual.sd =", dev.std, "robust.sd =", 
            dev.mad), "\n")
    if (output) 
        return(data.frame(arc.length, v.deviation = zdeviation, 
            x, y, xp, yp))
}
