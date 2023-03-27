fitCheckL3Spiral <-
function (x = x, y = y, m = NULL, axis.start = c(NULL, NULL), 
     start.values=c(a=2,k1=0.2,k2=0.2,k3=0.2), 
    specimen.name = " ", tolerance.step = 0.01, forward = 0, 
    back = 0, plot.ss = TRUE, plot.detail = TRUE, keep.output = FALSE, 
    no.error.messages = TRUE, orient.check = TRUE) 
{
    if (m == 999) 
        stop("Need to input A change location, or use fitCheckL2Spiral to find this location")
    xx <- x
    yy <- y
    N <- length(xx)
    if (is.null(m)) stop("Require input for change location (m)")
    requireNamespace("MASS")
    warn.msg1 <- "Outline is has a clockwise direction, have flipped to anticlockwise temporarily for computations"
    if (orient.check) {
        curvature <- curvatureNoPenalty(x = xx, y = yy, smooth.factor = 10)
        curve.direction <- median(curvature[2:10, 2])
        if (curve.direction < 0) {
            warning(warn.msg1)
            flip <- TRUE
            max.x <- max(xx)
            move.x <- max.x + 10
            xx <- -1 * xx + move.x
        }
    }
        dev.set(2)
        MASS::eqscplot(xx, yy, pch = 1)
          points(xx[1], yy[1], pch = 16, col = 2, cex = 1.1)
          points(xx[m], yy[m], pch = 15, col = 4, cex = 1.5)
if (forward !=0) points(xx[forward], yy[forward], pch=4,cex=2, col=4,lwd=2)
if (back !=0) points(xx[ (N-back)], yy[(N-back)], pch=4,cex=2, col=4,lwd=2)
if ( forward > m | (N-back) < m ) stop( "Error: values for forward or back overlap the given change location,
                                   must be before or after the change location see plot")

#
    if (is.null(axis.start)) {
        title("Please mouse (left) click initial L3 axis location", 
            cex = 0.9, col.main = 2, adj = 0)
        locate.xy <- locator(n = 1)
        start.u <- locate.xy$x
        start.v <- locate.xy$y
        points(start.u, start.v, pch=3, cex=3, col=4, lwd=2)
    }
    else {
        start.u <- axis.start[1]
        start.v <- axis.start[2]
points(start.u, start.v, pch=3, cex=3, col=4, lwd=2)
title("Outline with specified initial axis L3 spiral", 
            cex = 0.9, col.main = 2, adj = 0)
    }
    last <- length(xx)
    xstep1 <- seq(6 + forward, (m - 6), 1)
    xstep2 <- seq((m + 6), (last - back - 6), 1)
    xsteps <- c(xstep1, xstep2)
 #   xx <- xx[(1 + forward):(last - back)]
 #   yy <- yy[(1 + forward):(last - back)]
    lspiral.lhs <- function(x, y, u, v) sqrt((x - u)^2 + (y - 
        v)^2)
    lspiral.rhs <- function(x, y, u, v, a, k1, k2, k3, mc1 = m1, 
        mc2 = m2) {
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
    m1 <- m
# use L1 spiral to estimate variation, uses default L1 start values
#
    xfit1 <- fitL1Spiral(x = xx, y = yy, start.u = u, start.v = v)
    rss1 <- xfit1$parameters2["sd.deviations"]
#
# use L2 spiral to estimate variation with default L2 start values
    xfit2 <- fitL2Spiral(x = xx, y = yy, start.u = u, start.v = v, 
        m = m)
    rss2 <- xfit2$parameters2["sd.deviations"]
    mssd2 <- xfit2$parameters2["sd.msd"]
#

if ( length(start.values) !=4) stop ("Must input FOUR starting parameter values see help(fitL3Spiral)")
a <- start.values["a"]
k1 <- start.values["k1"]
k2 <- start.values["k2"]
k3 <- start.values["k3"]
# NOTE:  does not use the function fitL3Spiral in loop, but uses same code
#
    cat("Checking along outline points... progress in tens", 
        "\n")
    for (i in seq(along = xsteps)) {
        m.next <- xsteps[i]
        if (i%%10 == 0) 
            cat(i, " ")
        if (i%%150 == 0) 
            cat("\n")
        flush.console()
        if (m.next > m) {
            m2 <- m.next
            xfit3 <- try(nls(~lspiral.lhs(x, y, u, v) - lspiral.rhs(x, 
                y, u, v, a, k1, k2, k3), trace = F, control = list(tol = tolerance.step, 
                maxiter = 400), start = list(u = u, v = v, a = a, 
                k1 = k1, k2 = k2, k3 = k3)), silent = no.error.messages)
        }
        else {
            m1 <- m.next
            m2 <- m
            xfit3 <- try(nls(~lspiral.lhs(x, y, u, v) - lspiral.rhs(x, 
                y, u, v, a, k1, k2, k3), trace = F, control = list(tol = tolerance.step, 
                maxiter = 400), start = list(u = u, v = v, a = a, 
                k1 = k1, k2 = k2, k3 = k3)), silent = no.error.messages)
        }
        fit.class <- as.character(class(xfit3))
        if (fit.class == "try-error") 
            ss.usual[i] <- -999
        else ss.usual[i] <- summary(xfit3)$sigma
        ystep[i] <- m.next
        yseq[i] <- i
    }
    xout <- data.frame(cbind(yseq, ystep, ss.usual))
    xxout <- xout[xout$ss.usual > 0, ]
    xrss.min <- order(xxout$ss.usual)
    ystep.min <- round(xxout$ystep[xrss.min[1]], digits = 0)
    rss3.min <- xxout[xxout$ystep == ystep.min, "ss.usual"]
xfit3 <- fitL3Spiral(x = xx, y = yy, start.u = u, start.v = v, 
        m = sort(c(m, ystep.min)))
    axis.x <- xfit3$parameters1["x.axis"]
    axis.y <- xfit3$parameters1["y.axis"]
#
# placing the estimated axis location and second change point on outline
dev.set(2)
points(axis.x, axis.y, pch=10,col=2, cex=3,lwd=1.2)
points( xx[ystep.min], yy[ystep.min], pch=16, col=2, cex=2.5)
#
    if (plot.ss) {
dev.set(3)
        if (plot.detail) {
            ss.middle <- median(xxout$ss.usual)
            ss.range <- mad(xxout$ss.usual) * 8
            low.range <- max(0, ss.middle - ss.range)
            high.range <- ss.middle + ss.range
            plot(xxout$ystep, xxout$ss.usual, ylim = c(low.range, 
                high.range), ylab = "Residual standard deviation", 
                xlab = "Coordinate location along outline")
            axis(3, labels = F)
            axis(4, labels = F)
            abline(v = ystep.min, lwd = 1.5, col = 4)
            abline(h = rss3.min, col = 4)
            title(main = paste(specimen.name, " Second minimum at   ", 
                ystep.min), col.main = 4, font.main = 2, sub = paste("First minimum (given) at ", 
                m), cex.sub = 0.85, col.sub = 4)
        }
        else {
            ss.middle <- median(xxout$ss.usual)
            ss.range <- mad(xxout$ss.usual) * 8
            low.range <- min(xxout$ss.usual)
            low.range <- min(0, mssd2, xxout$ss.usual)
            high.range <- rss1 + (rss1/3)
            plot(xxout$ystep, xxout$ss.usual, ylim = c(low.range, 
                high.range), ylab = "Residual stanard deviation", 
                xlab = "Coordinate location along outline")
            axis(3, labels = F)
            axis(4, labels = F)
            abline(v = ystep.min, lwd = 1.5, col = 4)
            abline(h = rss1, col = 1, lty = 1, lwd = 2)
            abline(h = rss2, col = 2, lty = 2, lwd = 2)
            abline(h = mssd2, col = 3, lty = 1, lwd = 2)
            graphics::legend("topleft", c("1 spiral rss", "2 spiral rss", 
                "3 spiral mssd"), bty = "n", col = c(1, 2, 3), 
                lty = c(1, 2, 1), lwd = 2)
            title(main = paste(specimen.name, " Second minimum at   ", 
                ystep.min), col.main = 4, font.main = 2, sub = paste("First minimum (given) at ", 
                m), cex.sub = 0.85, col.sub = 4)
        }
    }
    cat("\n")
     minimum.location <- xxout[xxout$ystep == ystep.min, ]
    console.output <- data.frame(minimum.location, axis.x = axis.x, 
        axis.y = axis.y)
    print(console.output)
#
    if (keep.output) 
        return(xxout)
}
