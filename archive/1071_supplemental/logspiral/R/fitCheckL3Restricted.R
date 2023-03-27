fitCheckL3Restricted <-
function (x, y, m = NULL, axis.start = c(NULL, NULL), specimen.name = " ", 
    start.values=c(a=2,k1=0.2,k2=0.2,k3=0.2),
    tolerance.step = 0.01, from = NULL, to = NULL, plot.ss = TRUE, 
    keep.output = FALSE, no.error.messages = TRUE, orient.check = TRUE) 
{
    xx <- x
    yy <- y
    N <- length(xx)
    requireNamespace("MASS")
    warn.msg1 <- "Outline is has a clockwise direction, have flipped to anticlockwise temporarily for computations"
    curvature <- curvatureNoPenalty(x = xx, y = yy, smooth.factor = 10)
    curve.direction <- median(curvature[2:10, 2])
    if (curve.direction < 0) {
        warning(warn.msg1)
        flip <- TRUE
        max.x <- max(xx)
        move.x <- max.x + 10
        xx <- -1 * xx + move.x
    }
        dev.set(2)
        MASS::eqscplot(xx, yy, pch = 1)
     points(xx[1], yy[1], pch = 16, col = 2, cex = 1.1)
          points(xx[m], yy[m], pch = 15, col = 4, cex = 1.5)
if (!is.null(from)) points(xx[from], yy[from], pch=4,cex=2, col=4,lwd=2)
if (!is.null(to)) points(xx[ to], yy[to], pch=4,cex=2, col=4,lwd=2)
if ( is.null(from) | is.null(to) ) stop("Must input from and to locations on the outline")
#
if ( is.null(m) ) stop("Must input ONE change location (m) on the outline ")
#
    if (is.null(axis.start)) {
         title("Please mouse (left) click for initial L4 axis location", 
            cex = 0.9, col.main = 2, adj = 0)
         locate.xy <- locator(n = 1)
        start.u <- locate.xy$x
        start.v <- locate.xy$y
points( start.u, start.v, pch=3, cex=3, col=4, lwd=2)
    }
    else {
        start.u <- axis.start[1]
        start.v <- axis.start[2]
points(start.u, start.v, pch=3, cex=3, col=4, lwd=2)
title("Outline with specified initial L4 axis location", 
            cex = 0.9, col.main = 2, adj = 0)
    }
    xx <- x
    yy <- y
# check if from and to overlap the change location, m.
if ( (to - from) < 10 ) stop("must have to greater than from and difference of more than ten locations")
if ( from < m & to > m ) stop(" from & to values must BOTH be either before or after change location (m)")
#
if ( (from - 6) < 1) from <- 7
if ( (N - to) < 1 ) to <- N-7
if ( (m - to) < 6 ) to <- to - 7
if ( (to - m) < 6 ) to <- to + 7 
#
    xsteps <- seq(from, to, 1)  
#
    lspiral.lhs <- function(x, y, u, v) sqrt((x - u)^2 + (y - 
        v)^2)
    lspiral.rhs1 <- function(x, y, u, v, a, k1, k2, k3, mc1 = m1, 
        mc2 = m2) {
        xtheta <- cumulativeAngle(x, y, u, v)
        xc1 <- xtheta[mc1]
        xc2 <- xtheta[mc2]
        a * exp(k1 * pmin(xtheta, xc1)) * exp(k2 * pmin(xc2 - 
            xc1, pmax(0, xtheta - xc1))) * exp(k3 * pmax(xtheta - 
            xc2, 0))
    }
   lspiral.rhs2 <- function(x, y, u, v, a, k1, k2, k3, mc1 = m2, 
        mc2 = m1) {
        xtheta <- cumulativeAngle(x, y, u, v)
        xc1 <- xtheta[mc1]
        xc2 <- xtheta[mc2]
        a * exp(k1 * pmin(xtheta, xc1)) * exp(k2 * pmin(xc2 - 
            xc1, pmax(0, xtheta - xc1))) * exp(k3 * pmax(xtheta - 
            xc2, 0))
    }

    ss.usual <- rep(NA, length(xsteps))
    ystep <- rep(0, length(xsteps))
    yseq <- rep(0, length(xsteps))
    u <- start.u
    v <- start.v
#
if ( length(start.values) !=4) stop ("Must input FOUR starting parameter values see help(fitL3Spiral)")
a <- start.values["a"]
k1 <- start.values["k1"]
k2 <- start.values["k2"]
k3 <- start.values["k3"]
#  
 m1 <- m
    cat("Checking outline points... progress in tens", "\n")
    for (i in seq(along = xsteps)) {
       m2 <- xsteps[i]
        if (i%%10 == 0) 
            cat(i, " ")
        if (i%%150 == 0) 
            cat("\n")
        flush.console()
 if ( m1 < m2 )
    {
        xfit2 <- try(nls(~lspiral.lhs(x, y, u, v) - lspiral.rhs1(x, 
            y, u, v, a, k1, k2, k3), trace = F, control = list(tol = tolerance.step, 
            maxiter = 400), start = list(u = u, v = v, a = a, 
            k1 = k1, k2 = k2, k3 = k3)), silent = no.error.messages)
}
else
{
       xfit2 <- try(nls(~lspiral.lhs(x, y, u, v) - lspiral.rhs2(x, 
            y, u, v, a, k1, k2, k3), trace = F, control = list(tol = tolerance.step, 
            maxiter = 400), start = list(u = u, v = v, a = a, 
            k1 = k1, k2 = k2, k3 = k3)), silent = no.error.messages)
}

        fit.class <- as.character(class(xfit2))
        if (fit.class == "try-error") 
            ss.usual[i] <- -999
        else ss.usual[i] <- summary(xfit2)$sigma
        ystep[i] <- m2
        yseq[i] <- i
    }
    xout <- data.frame(cbind(yseq, ystep, ss.usual))
    xxout <- xout[xout$ss.usual > 0, ]
    xrss.min <- order(xxout$ss.usual)
    ystep.min <- round(xxout$ystep[xrss.min[1]], digits = 0)
#
if ( m1 > ystep.min)
{
xfit3 <- fitL3Spiral(x = xx, y = yy, start.u = u, start.v = v, 
        m = c( ystep.min, m1))
}
else
{
xfit3 <- fitL3Spiral(x = xx, y = yy, start.u = u, start.v = v, 
        m = c(  m1, ystep.min))

}
    axis.x <- xfit3$parameters1["x.axis"]
    axis.y <- xfit3$parameters1["y.axis"]
#
#
# placing the estimated axis location and second change point on outline
dev.set(2)
points(axis.x, axis.y, pch=10,col=2, cex=3,lwd=1.2)
points( xx[ystep.min], yy[ystep.min], pch=16, col=2, cex=2.5)
#

    if (plot.ss) {
	dev.set(3)
        ss.middle <- median(xxout$ss.usual)
        ss.range <- mad(xxout$ss.usual) * 8
        low.range <- min(xxout$ss.usual)
        high.range <- ss.middle + ss.range
        plot(xxout$ystep, xxout$ss.usual, ylim = c(low.range, 
            high.range), ylab = "Residual stanard deviation", 
            xlab = "Coordinate location along outline")
        axis(3, labels = F)
        axis(4, labels = F)
        abline(v = ystep.min, lwd = 1.5, col = 4)
        title(main = paste(specimen.name, " Second minimum at   ", 
            ystep.min), col.main = 4, font.main = 2, sub = paste("First minimum (given) at ", 
            m), cex.sub = 0.85, col.sub = 4)
    }
    cat("\n")
    minimum.location <- xxout[xxout$ystep == ystep.min, ]
console.output <- data.frame(minimum.location, axis.x=axis.x,
                   axis.y=axis.y)
    print(console.output)
    if (keep.output) 
        return(xxout)
}
