plotDiagnostics <-
function (spiral.output = spiral.output, change = c(NULL, NULL, 
    NULL), output.device = 5) 
{
    if (is.null(change)) 
        m <- 0
    else m <- length(change) + 1
    dev.set(output.device)
    old.par <- par(no.readonly = TRUE)
    par(mfrow = c(2, 2))
    deviations <- spiral.output$deviations
    ndev <- length(deviations)
    arc.length <- spiral.output$arc.length
    angle <- spiral.output$angle
    fitted.r <- spiral.output$fitted
    chge1 <- change[1]
    chge2 <- change[2]
    chge3 <- change[3]
    up.limit <- 2.5 * spiral.output$parameters2[3]
    low.limit <- -1 * up.limit
    range1 <- diff(range(deviations))
    range2 <- diff(range(c(up.limit, low.limit)))
    if (range2 > range1) {
        plot(fitted.r, deviations, type = "s", xlab = "Fitted distance from spiral axis", 
            ylab = "Spiral deviation", ylim = c(low.limit, up.limit))
    }
    else {
        plot(fitted.r, deviations, type = "s", xlab = "Fitted distance from spiral axis", 
            ylab = "Spiral deviation")
    }
    abline(h = 0, lwd = 2, col = 4)
    abline(h = c(up.limit, low.limit), col = 2)
    abline(v = arc.length[c(chge1, chge2, chge3)], col = 4, lwd = 2)
    axis(4, labels = F)
    axis(3, labels = F)
    title(main = "Radial deviations & distance", sub = "Change points (if any) are vertical lines", 
        col.sub = 4)
    plot(angle, log(fitted.r), xlab = "Angle (radians)", ylab = "Log distance from spiral axis")
    lines(angle, log(fitted.r), col=2, lwd=1.5)  
    abline(v = angle[c(chge1, chge2, chge3)], col = 4, lwd = 2)
    title(main = "Fitted distance & angle", sub = "Change points (if any) are vertical lines", 
        col.sub = 4)
    qqnorm(deviations, ylab = "Spiral deviations")
    axis(4, labels = F)
    qqline(deviations, lwd = 1.8, col = 4)
    cpgram(deviations, main = "Cumulative periodogram")
    axis(3, labels = F)
    axis(3, at = seq(0, 0.45, 0.05), labels = F, tck = -0.01)
    axis(1, at = seq(0, 0.45, 0.05), labels = F, tck = -0.01)
    text(0.25, 0.1, paste("N = ", ndev), adj = 0)
    par(old.par)
#
# check for extreme deviations around change locations (+/- 4 coords and 95% quantile)
#
dev95 <- quantile( abs(deviations), prob=0.95)
if (!is.null(chge1) & length(change) > 0 )
if( max(deviations[(chge1-4):(chge1+4)] ) > dev95) warning("Extreme deviation(s) at or near 1st change? Check spiral shape",call. = FALSE)
if (!is.null(chge2) & length(change) > 1 )
if( max(deviations[(chge2-4):(chge2+4)] ) > dev95) warning("Extreme deviation(s) at or near 2nd change? Check spiral shape",,call. = FALSE)
if (!is.null(chge3) & length(change) > 2 )
if( max(deviations[(chge3-4):(chge3+4)] ) > dev95) warning("Extreme deviation(s) at or near 3rd change? Check spiral shape",,call. = FALSE)

}
