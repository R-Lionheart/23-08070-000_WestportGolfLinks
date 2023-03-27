fitCheckL4Spiral <-
function (x, y, m = c(NULL, NULL), axis.start = c(NULL, NULL), 
    start.values=c(a=2,k1=0.2,k2=0.2,k3=0.2,k4=0.2), 
    specimen.name = " ", tolerance.step = 0.01, forward = 0, 
    back = 0, plot.detail = TRUE, plot.ss = TRUE, keep.output = FALSE, 
    no.error.messages = TRUE, orient.check = TRUE) 
{
    if (is.null(m)) 
        stop("Need to input two change locations, or use fitCheckL3Spiral to find these locations")
else
    {  
          m1 <- m[1]
          m2 <- m[2]
    }
    xx <- x
    yy <- y
	N <- length(xx)
    requireNamespace("MASS")
    if (orient.check) {
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
    } 
       dev.set(2)
        MASS::eqscplot(xx, yy, pch = 1)
       points(xx[1], yy[1], pch = 16, col = 2, cex = 1.1)
         points(xx[m1], yy[m1], pch = 15, col = 4, cex = 1.5)
         points(xx[m2], yy[m2], pch = 15, col = 4, cex = 1.5)
	if (forward !=0) points(xx[forward], yy[forward], pch=4,cex=2, col=4,lwd=2)
	if (back !=0) points(xx[ (N-back)], yy[(N-back)], pch=4,cex=2, col=4,lwd=2)
if ( (N-back) < m2 ) warning("May get error: check if forward and back values overlap with change locations")
if ( forward > m1  ) warning("May get error: check if forward and back values overlap with change locations")
#
    if (is.null(axis.start)) {
        title("Please mouse (left) click initial L4 axis location", 
            cex = 0.8, col.main = 2, adj = 0)
        locate.xy <- locator(n = 1)
        start.u <- locate.xy$x
        start.v <- locate.xy$y
	points(start.u, start.v, pch=3, cex=2.5, col=4, lwd=2.5)
    }
    else {
        start.u <- axis.start[1]
        start.v <- axis.start[2]
        points(start.u, start.v, pch=3, cex=2.5, col=4, lwd=2.5)
	title("Outline with specified initial L4 axis location", 
            cex = 0.9, col.main = 2, adj = 0)
    }
# setting up the steps for fitting the L4 spiral
#
 warn.msg2 <- "First change too close to start to check for L4 spiral"
 warn.msg3 <- "Second change too close to end to check for L4 spiral"

    last <- length(xx)
if ( (m[1] + forward) < 10 ) 
{ 
 warning(warn.msg2) 
xstep1 <- NULL
}
 else     
xstep1 <- seq((6 + forward), (m[1] - 6), 1)
#
    xstep2 <- seq((m[1] + 6), (m[2] - 6), 1)
#
if (  (last -m[2] - back) < 10 ) 
{ 
 warning(warn.msg3) 
xstep3 <- NULL
}
else 
   xstep3 <- seq((m[2] + 6), (last - back - 6), 1)
#
    xsteps <- c(xstep1, xstep2, xstep3)
    lspiral.lhs <- function(x, y, u, v) sqrt((x - u)^2 + (y - 
        v)^2)
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
    ss.usual <- rep(NA, length(xsteps))
    ystep <- rep(0, length(xsteps))
    yseq <- rep(0, length(xsteps))
    u <- start.u
    v <- start.v
#
# L3 spiral used for estimating variance
    xfit3 <- fitL3Spiral(x = xx, y = yy, start.u = u, start.v = v, 
        m = c(m[1], m[2]))
    rss3 <- xfit3$parameters2["sd.deviations"]
    mssd3 <- xfit3$parameters2["sd.msd"]
#
if ( length(start.values) !=5) stop ("Must input FIVE starting parameter values see help(fitL4Spiral)")
a <- start.values["a"]
k1 <- start.values["k1"]
k2 <- start.values["k2"]
k3 <- start.values["k3"]
k4 <- start.values["k4"]
#  NOTE: fitL4Spiral function not used, but code is used.

    for (i in seq(along = xsteps)) {
        m.next <- xsteps[i]
        if (i%%10 == 0) 
            cat(i, " ")
        if (i%%150 == 0) 
            cat("\n")
        flush.console()
        if (m.next < m[1]) {
            m1 <- m.next
            m2 <- m[1]
            m3 <- m[2]
            xfit4 <- try(nls(~lspiral.lhs(x, y, u, v) - lspiral.rhs(x, 
                y, u, v, a, k1, k2, k3, k4), trace = F, control = list(tol = tolerance.step, 
                maxiter = 400), start = list(u = u, v = v, a = 2, 
                k1 = 0.2, k2 = 0.2, k3 = 0.2, k4 = 0.2)), silent = no.error.messages)
        }
        else {
            if (m.next > m[1] & m.next < m[2]) {
                m1 <- m[1]
                m2 <- m.next
                m3 <- m[2]
                xfit4 <- try(nls(~lspiral.lhs(x, y, u, v) - lspiral.rhs(x, 
                  y, u, v, a, k1, k2, k3, k4), trace = F, control = list(tol = tolerance.step, 
                  maxiter = 400), start = list(u = u, v = v, 
                  a = 2, k1 = 0.2, k2 = 0.2, k3 = 0.2, k4 = 0.2)), 
                  silent = no.error.messages)
            }
            else {
                m1 <- m[1]
                m2 <- m[2]
                m3 <- m.next
                xfit4 <- try(nls(~lspiral.lhs(x, y, u, v) - lspiral.rhs(x, 
                  y, u, v, a, k1, k2, k3, k4), trace = F, control = list(tol = tolerance.step, 
                  maxiter = 400), start = list(u = u, v = v, 
                  a = 2, k1 = 0.2, k2 = 0.2, k3 = 0.2, k4 = 0.2)), 
                  silent = no.error.messages)
            }
        }
        fit.class <- as.character(class(xfit4))
        if (fit.class == "try-error") 
            ss.usual[i] <- -999
        else ss.usual[i] <- summary(xfit4)$sigma
        ystep[i] <- xsteps[i]
        yseq[i] <- i
    }
    xout <- data.frame(cbind(yseq, ystep, ss.usual))
    xxout <- xout[xout$ss.usual > 0, ]
    xrss.min <- order(xxout$ss.usual)
    ystep.min <- round(xxout$ystep[xrss.min[1]], digits = 0)

xfit4 <- fitL4Spiral(x = xx, y = yy, start.u = u, start.v = v, 
        m = sort(c(m1,m2, ystep.min)))
    axis.x <- xfit4$parameters1["x.axis"]
    axis.y <- xfit4$parameters1["y.axis"]

#
# place estimates of axis and change on the outline
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
                high.range), ylab = "Residual stanard deviation", 
                xlab = "Coordinate location along outline")
            axis(3, labels = F)
            axis(4, labels = F)
            abline(v = ystep.min, lwd = 1.5, col = 4)
            title(main = paste(specimen.name, " THIRD minimum at   ", 
                ystep.min), col.main = 4, font.main = 2, sub = paste("Two minima (given) at ", 
                m[1], m[2]), cex.sub = 0.85, col.sub = 4)
        }
        else {
            low.range <- min(0, mssd3 - (mssd3/2))
            high.range <- rss3 + (rss3/3)
            plot(xxout$ystep, xxout$ss.usual, ylim = c(low.range, 
                high.range), ylab = "Residual stanard deviation", 
                xlab = "Coordinate location along outline")
            axis(3, labels = F)
            axis(4, labels = F)
            abline(v = ystep.min, lwd = 1.5, col = 4)
            abline(h = rss3, col = 1, lty = 2, lwd = 2)
            abline(h = mssd3, col = 3, lty = 1, lwd = 2)
            graphics::legend("topleft", c("3 spiral rss", "3 spiral mssd"), 
                bty = "n", col = c(1, 3), lty = c(2, 1), lwd = 2)
            title(main = paste(specimen.name, " Third minimum at   ", 
                ystep.min), col.main = 4, font.main = 2, sub = paste("Two minima (given) at ", 
                m[1], m[2]), cex.sub = 0.85, col.sub = 4)
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
