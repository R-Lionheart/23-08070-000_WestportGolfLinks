doLocateChange <-
function (spiral.output = spiral.output, nc = NULL, eps.factor = 1) 
{
    length.names <- length(names(spiral.output))
    if (length.names == 6) {
        arc <- spiral.output$arc
        ssd <- spiral.output$no.noise
    }
    else {
        arc <- spiral.output$arc.length
        ssd <- spiral.output$v.deviations
    }
    plot(arc, ssd, type = "s", xlab = "Arc length", ylab = "VERTICAL deviations")
    change <- locator(n = nc)
    eps <- eps.factor * median(diff(arc))
    abline(v = change$x, col = 3, lwd = 2)
    locate.change <- NULL
    for (ii in seq(1, nc)) {
        locate.change[ii] <- (which(arc < change$x[ii] + eps & 
            arc > change$x[ii] - eps))[1]
    }
    return(locate.change)
}
