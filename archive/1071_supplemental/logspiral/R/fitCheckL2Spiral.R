fitCheckL2Spiral <-
function (x, y, axis.start = c(NULL, NULL),  
    start.values =c(a=2, k1=0.2, k2=0.2), specimen.name = " ",
    tolerance.step = 0.1, forward = 0, back = 0, plot.ss = TRUE, 
    plot.detail = FALSE, keep.output = FALSE, no.error.messages = TRUE, 
    orient.check = TRUE) 
{
    xx <- x
    yy <- y
    N <- length(xx)
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
if (forward !=0) points(xx[forward], yy[forward], pch=4,cex=2, col=4,lwd=2)
if (back !=0) points(xx[ (N-back)], yy[(N-back)], pch=4,cex=2, col=4,lwd=2)

    if (is.null(axis.start)) {
        title("Please mouse (left) click initial L2 axis location", 
            cex = 0.9, col.main = 2, adj = 0)    
        locate.xy <- locator(n = 1)
        start.u <- locate.xy$x
        start.v <- locate.xy$y
points(start.u, start.v, pch=3, cex=2.5, col=4, lwd=2.5)
    }
    else {
        start.u <- axis.start[1]
        start.v <- axis.start[2]
points(start.u, start.v, pch=3, cex=2.5, col=4, lwd=2.5)
title("Outline with specified initial L2 axis location", 
            cex = 0.9, col.main = 2, adj = 0)
    }
    xsteps <- seq(1 + forward + 6, length(x) - back - 6, 1)
#    last <- length(x) - back
#    begin <- 1 + forward
#    xx <- xx[begin:last]
#    yy <- yy[begin:last]
#
    lspiral.lhs <- function(x, y, u, v) sqrt((x - u)^2 + (y - 
        v)^2)
    ss.usual <- rep(NA, length(xsteps))
    ystep <- rep(0, length(xsteps))
    yseq <- rep(0, length(xsteps))
    u <- start.u
    v <- start.v
# single linear spiral only used for estimating variation
# with default values for start.values (see fitL1Spiral)
    xfit1 <- fitL1Spiral(x = xx, y = yy, start.u = u, start.v = v)
    rss1 <- xfit1$parameters2["sd.deviations"]
    mssd1 <- xfit1$parameters2["sd.msd"]
#
# two spiral fitting along outline starts here. Note fitL2Spiral not used in loop
#
    lspiral.rhs <- function(x, y, u, v, a, k1, k2, m = mm) {
        xtheta <- cumulativeAngle(x, y, u, v)
        xchange <- xtheta[m]
        a * exp(k1 * pmin(xtheta, xchange)) * exp(k2 * pmax(xtheta - 
            xchange, 0))
    }

if ( length(start.values) !=3) stop ("Must input THREE starting parameter values see help(fitL2Spiral)")
a <- start.values["a"]
k1 <- start.values["k1"]
k2 <- start.values["k2"]
#
    cat("Checking outline points... progress in tens", "\n")
    for (i in seq(along = xsteps)) {
        mm <- xsteps[i]
        if (i%%10 == 0) 
            cat(i, " ")
        if (i%%150 == 0) 
            cat("\n")
        flush.console()
        xfit2 <- try(nls(~lspiral.lhs(xx, yy, u, v) - lspiral.rhs(xx, 
            yy, u, v, a, k1, k2), trace = F, control = list(tol = tolerance.step, 
            maxiter = 400), start = list(u = u, v = v, a = a, 
            k1 = k1, k2 = k2)), silent = no.error.messages)
        fit.class <- as.character(class(xfit2))
        if (fit.class == "try-error") 
            ss.usual[i] <- -999
        else {
            fit.summary <- summary(xfit2)
            ss.usual[i] <- fit.summary$sigma
        }
        ystep[i] <- xsteps[i]
        yseq[i] <- i
    }
    xout <- data.frame(cbind(yseq, ystep, ss.usual))
    xxout <- xout[xout$ss.usual > 0, ]
    xrss.min <- order(xxout$ss.usual)
    ystep.min <- round(xxout$ystep[xrss.min[1]], digits = 0)
    xfit2 <- fitL2Spiral(x = xx, y = yy, start.u = u, start.v = v, 
        m = ystep.min)
    rss2 <- xfit2$parameters2["sd.deviations"]
    mssd2 <- xfit2$parameters2["sd.msd"]
    axis.x <- xfit2$parameters1["x.axis"]
    axis.y <- xfit2$parameters1["y.axis"]
# 
# place estimates of axis and change on the outline
#
dev.set(2)
points( axis.x, axis.y, pch=10, col=2, cex=3)
points( xx[ystep.min], yy[ystep.min], pch=16, col=2, cex=2)
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
            title(paste(specimen.name, " Minimum at   ", ystep.min, 
                " for TWO spiral fit"), adj = 0.01, cex = 0.7, 
                col = 2)
        }
        else {
            low.range <- mssd2 - (mssd2/2)
            if (low.range < 0.1) 
                low.range <- 0
            high.range <- rss1 + (rss1/3)
            plot(xxout$ystep, xxout$ss.usual, ylim = c(low.range, 
                high.range), ylab = "Residual standard deviation", 
                xlab = "Coordinate location along outline")
            axis(3, labels = F)
            axis(4, labels = F)
            abline(v = ystep.min, lwd = 1.5, col = 4)
            abline(h = rss1, col = 1, lty = 1)
            abline(h = mssd1, col = 1, lty = 2)
            abline(h = rss2, col = 4, lty = 1)
            abline(h = mssd2, col = 3, lty = 1)
            graphics::legend("topleft", c("1 spiral rss", "1 spiral mssd", 
                " 2 spiral mssd"), bty = "n", col = c(1, 1, 3), 
                lty = c(1, 2, 1), lwd = 2)
            title(paste(specimen.name, " Minimum at   ", ystep.min, 
                " for TWO spiral fit"), adj = 0.01, cex = 0.7, 
                col = 2)
        }
    }
    cat("\n")
    minimum.location <- xxout[xxout$ystep == ystep.min, ]
    console.output <- data.frame(minimum.location, axis.x = axis.x, 
        axis.y = axis.y)
    print(console.output)
    if (keep.output) 
        return(xxout)
}
