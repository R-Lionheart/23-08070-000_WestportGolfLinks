bestSmooth <-
function (x, y, start = 0.01, end = 0.98, delta = 0.005, target = NULL, 
    plot.it = FALSE) 
{
    steps <- seq(start, end, delta)
    out <- NULL
    for (ii in seq(along = steps)) out[ii] <- arcSmooth2D(x, 
        y, smooth.factor = steps[ii], summary = TRUE)[4]
    if (is.null(target)) {
        dev.curvature <- curvatureNoPenalty(steps, out)
        best <- steps[which.max(dev.curvature$curvature)]
        if (plot.it) {
            dev.set(2)
            plot(steps, out, xlab = "Smooth factor(spar)", ylab = "Robust residual sd")
            abline(v = best, col = 4, lwd = 2)
            dev.set(3)
            plot(steps, dev.curvature$curvature, xlab = "Smooth.factor(spar)", 
                ylab = "Curvature of robust sd")
            abline(v = best, col = 4, lwd = 2)
        }
    }
    else {
        step.out <- data.frame(steps, out)
        step.out <- step.out[step.out$out < target, ]
        best <- step.out$steps[length(step.out$steps)]
        if (plot.it) {
            dev.set(2)
            plot(steps, out, xlab = "Smooth factor(spar)", ylab = "Robust residual sd")
            abline(v = best, col = 4, lwd = 2)
            abline(h = target, col = 2, lwd = 2)
        }
    }
    return(best)
}
