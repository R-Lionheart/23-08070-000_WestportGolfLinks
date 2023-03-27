generateAnyLinear <-
function (nstart = 200, change = c(50, 100, 150), rate = 0.4, 
    k1 = 35, k2 = 35, k3 = 35, k4 = 35, dist.zero = 2, noise = 0.01, 
    noise.increase = FALSE, orient = "NO", distance = "far", 
    angle = 0, plane = "X", plot = T, waves = 1, show.changes = TRUE, 
    theta.start = 0.001) 
{
    rotate <- function(xx, yy, theta, xzero = 0, yzero = 0, degree = T) {
        radians <- theta * (pi/180)
        h <- xzero
        k <- yzero
        xr <- (xx - h) * cos(radians) + (yy - k) * sin(radians)
        yr <- (yy - k) * cos(radians) - (xx - h) * sin(radians)
        xp <- xr + h
        yp <- yr + k
        list(x = xp, y = yp)
    }
    if (noise > 0.5 * rate) 
        warning("Noise > 0.5*arc increment, reduce incr. or change rate. See Help")
cat( "Spiral angles :  ", c(k1,k2,k3,k4), "  Change (if any) at : ",change, "\n")
# in radians
    k1 <- 1/(tan(k1 * (pi/180)))
    k2 <- 1/(tan(k2 * (pi/180)))
    k3 <- 1/(tan(k3 * (pi/180)))
    k4 <- 1/(tan(k4 * (pi/180)))
    xr <- NULL
    xtheta <- NULL
    xr[1] <- dist.zero
    xtheta[1] <- theta.start
    for (i in seq(2, nstart)) {
        if (i <= change[1]) {
            xr[i] <- xr[i - 1] + rate * (k1/sqrt(1 + k1^2))
            xtheta[i] <- xtheta[i - 1] + (log(xr[i]) - log(xr[i - 
                1]))/k1
        }
        else if (i > change[1] & i <= change[2]) {
            xr[i] <- xr[i - 1] + rate * (k2/sqrt(1 + k2^2))
            xtheta[i] <- xtheta[i - 1] + (log(xr[i]) - log(xr[i - 
                1]))/k2
        }
        else if (i > change[2] & i <= change[3]) {
            xr[i] <- xr[i - 1] + rate * (k3/sqrt(1 + k3^2))
            xtheta[i] <- xtheta[i - 1] + (log(xr[i]) - log(xr[i - 
                1]))/k3
        }
        else {
            xr[i] <- xr[i - 1] + rate * (k4/sqrt(1 + k4^2))
            xtheta[i] <- xtheta[i - 1] + (log(xr[i]) - log(xr[i - 
                1]))/k4
        }
    }
    if (noise.increase) {
        xr.limit <- xr/max(xr)
        xr <- xr + xr.limit * rnorm(nstart, 0, noise)
    }
    else xr <- xr + rnorm(nstart, 0, noise)
    if (waves > 1) {
        period <- round(nstart/waves, 0)
        xt <- 1:nstart
        xr.cycle <- 0.2 * cos((xt * 2 * pi)/period) + 0.12 * 
            sin((xt * 2 * pi)/period)
        xxr <- xr + xr.cycle
        x <- xxr * cos(xtheta)
        y <- xxr * sin(xtheta)
    }
    else {
        x <- xr * cos(xtheta)
        y <- xr * sin(xtheta)
    }
    axis.locate <- c(-min(x) + 10, -min(y) + 10)
    xx <- x - min(x) + 10
    yy <- y - min(y) + 10
    if (orient == "YES") {
        zz <- rotate(xx, yy, theta = -50)
        xxx <- zz$x - min(zz$x) + 10
        yyy <- zz$y + 10
        axis.new <- rotate(xx = axis.locate[1], yy = axis.locate[2], 
            theta = -50)
        axis.new$x <- axis.new$x - min(zz$x) + 10
        axis.new$y <- axis.new$y + 10
        axis.locate[1] <- axis.new$x
        axis.locate[2] <- axis.new$y
    }
    else {
        xxx <- xx
        yyy <- yy
    }
    if (distance == "far") {
        xxx <- xxx + 100
        yyy <- yyy + 100
        axis.locate <- axis.locate + 100
    }
    if (plane == "X") {
        xxx <- xxx * cos(angle * (pi/180))
        yyy <- yyy
    }
    else {
        xxx <- xxx
        yyy <- yyy * cos(angle * (pi/180))
    }
    cat("spiral axis at :  ", round(axis.locate, 2), "\n")
    cat("spiral parameters: ", c(dist.zero, k1, k2, k3, k4), 
        "\n")
    xs <- xxx
    ys <- yyy
    arc.length <- sqrt(diff(xs)^2 + diff(ys)^2)
    arc.diff <- arc.length
    arc.length <- c(0, cumsum(arc.length))
    arc.avg <- median(arc.diff)
    arc.sd <- sqrt(var(arc.diff))
    arc <- max(arc.length)
    cat("ARC: avg incr., sd incr., total length:  ", round(c(arc.avg, 
        arc.sd, arc), 4), "\n")
    if (plot) 
        requireNamespace("MASS")
    dev.set(2)
    MASS::eqscplot(xs, ys, pch = 16, cex = 0.5, col = 2, xlab = "", 
        ylab = "")
    points(axis.locate[1], axis.locate[2], pch = 10, cex = 2, 
        col = 4)
    axis(3, labels = F)
    axis(4, labels = F)
    if (show.changes) 
        points(xs[change], ys[change], cex = 0.7, col = 4, pch = 15)
    xy <- data.frame(x = xs, y = ys, logr = log(xr), angle = xtheta, 
        arc.length = arc.length)
    xy
}
