fitAnyLinear <-
function (x, y, m = c(NULL, NULL, NULL), axis.start = c(NULL, 
    NULL), specimen = "", res1 = NULL, res2 = NULL, tolerance.step = 0.01, 
    flip = FALSE, plot.fit = FALSE, plot.deviations = FALSE, 
    plot.diagnostics = FALSE, print.input = TRUE, orient.check = TRUE) 
{
    startx <- x[1]
    starty <- y[1]
    xx <- x
    yy <- y
    requireNamespace("MASS")
    warn.msg1 <- "Outline is has a clockwise direction, so temporary flip to anticlockwise for computations"
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
	  points( xx[m], yy[m], pch=16, col=4, cex=2.5)
    if (is.null(axis.start)) {
        title("Mouse (left) click best guess at axis location", 
            cex = 0.9, col.main = 2, adj = 0)
        locate.xy <- locator(n = 1)
        start.u <- locate.xy$x
        start.v <- locate.xy$y
	  points(start.u, start.v, pch=3, cex=2.5,col=4)
    }
    else {
        start.u <- axis.start[1]
        start.v <- axis.start[2]
points(start.u, start.v, pch=3, cex=2.5, col=4)
title("Outline with specified initial axis location", 
            cex = 0.9, col.main = 2, adj = 0)
    }
    dev.fit <- 3
    dev.deviation <- 4
    dev.diagnostics <- 5
    if (is.null(m)) 
        episodes <- 1
    else episodes <- length(m) + 1
    if (print.input) {
        cat("episodes = ", episodes, "\n")
        if (!is.null(m)) 
            cat("change(s) at  ", m, "\n")
    }
    if (episodes == 1) {
        spiral.output <- fitL1Spiral(x = xx, y = yy, start.u = start.u, 
            start.v = start.v,  flip.x = flip, move.by = move.x, 
            tolerance.step = tolerance.step, angle.deg=TRUE)
        if (plot.fit) 
            plotSpiralFit(spiral.output = spiral.output, title.label = specimen, 
                xdata = x, ydata = y, output.device = dev.fit)
        if (plot.deviations) 
            plotSpiralDeviations(spiral.output = spiral.output, 
                title.label = specimen, output.device = dev.deviation)
        if (plot.diagnostics) {
            plotDiagnostics(spiral.output = spiral.output, change = c(NULL, 
                NULL, NULL), output.device = dev.diagnostics)
        }
dev.set(2)
points( spiral.output$parameters1[1],spiral.output$parameters1[2],pch=10, cex=2.5, col=2)

        return(spiral.output)
    }
    if (episodes == 2) {
        spiral.output <- fitL2Spiral(x = xx, y = yy, m = m[1], 
            start.u = start.u, start.v = start.v, flip.x = flip, 
            move.by = move.x, tolerance.step = tolerance.step, angle.deg=TRUE)
        if (plot.fit) 
            plotSpiralFit(spiral.output = spiral.output, change = c(m[1]), 
                title.label = specimen, xdata = x, ydata = y, 
                output.device = dev.fit)
        if (plot.deviations) 
            plotSpiralDeviations(spiral.output = spiral.output, 
                change = c(m[1]), title.label = specimen, output.device = dev.deviation)
        if (plot.diagnostics) {
            plotDiagnostics(spiral.output = spiral.output, change = c(m[1], 
                NULL, NULL), output.device = dev.diagnostics)
        }
dev.set(2)
points( spiral.output$parameters1[1],spiral.output$parameters1[2],pch=10, cex=2.5, col=2)

        return(spiral.output)
    }
    if (episodes == 3) {
        m1 <- m[1]
        m2 <- m[2]
        if (m1 >= m2) 
            stop("*** wrong m was input: change  1 should be LESS than 2 for a THREE spiral fit")
        spiral.output <- fitL3Spiral(x = xx, y = yy, m = c(m1, 
            m2), start.u = start.u, start.v = start.v, flip.x = flip, 
            move.by = move.x, tolerance.step = tolerance.step, angle.deg=TRUE)
        if (plot.fit) 
            plotSpiralFit(spiral.output = spiral.output, change = c(m1, 
                m2), title.label = specimen, xdata = x, ydata = y, 
                output.device = dev.fit)
        if (plot.deviations) 
            plotSpiralDeviations(spiral.output = spiral.output, 
                change = c(m1, m2), title.label = specimen, output.device = dev.deviation)
        if (plot.diagnostics) {
            plotDiagnostics(spiral.output = spiral.output, change = c(m1, 
                m2, NULL, NULL), output.device = dev.diagnostics)
        }
dev.set(2)
points( spiral.output$parameters1[1],spiral.output$parameters1[2],pch=10, cex=2.5, col=2)

        return(spiral.output)
    }
    if (episodes == 4) {
        m1 <- m[1]
        m2 <- m[2]
        m3 <- m[3]
        if (m1 > m2 | m2 > m3) 
            stop("*** wrong m was input: change locations must be  STRICTLY increasing with the FOUR spiral fit")
        spiral.output <- fitL4Spiral(x = xx, y = yy, m = c(m1, 
            m2, m3), start.u = start.u, start.v = start.v, flip.x = flip, 
            move.by = move.x, tolerance.step = tolerance.step, angle.deg=TRUE)
        if (plot.fit) 
            plotSpiralFit(spiral.output = spiral.output, change = c(m1, 
                m2, m3), title.label = specimen, xdata = x, ydata = y, 
                output.device = dev.fit)
        if (plot.deviations) 
            plotSpiralDeviations(spiral.output = spiral.output, 
                change = c(m1, m2, m3), title.label = specimen, 
                output.device = dev.deviation)
        if (plot.diagnostics) {
            plotDiagnostics(spiral.output = spiral.output, change = c(m1, 
                m2, m3), output.device = dev.diagnostics)
        }
dev.set(2)
points( spiral.output$parameters1[1],spiral.output$parameters1[2],pch=10, cex=2.5, col=2)
        return(spiral.output)
    }
}
