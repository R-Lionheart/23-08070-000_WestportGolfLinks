curvatureNoPenalty <-
function (x, y, smooth.factor = 5) 
{
# derivatives and curvature
    arc.segment <- sqrt((diff(x)^2 + diff(y)^2))
    arc.length <- cumsum(c(0, arc.segment))
    zx <- smooth.spline(arc.length, x, df = smooth.factor)
    zy <- smooth.spline(arc.length, y, df = smooth.factor)
    zdx1 <- predict(zx, deriv = 1, x = arc.length)$y
    zdx2 <- predict(zx, deriv = 2, x = arc.length)$y
    zdy1 <- predict(zy, deriv = 1, x = arc.length)$y
    zdy2 <- predict(zy, deriv = 2, x = arc.length)$y
    curvature <- (zdx1 * zdy2 - zdx2 * zdy1)/(zdx1^2 + zdy1^2)^1.5
    out.df <- data.frame(arc.length, curvature, zdx1, zdx2, zdy1, 
        zdy2)
    names(out.df) <- c("arc", "curvature", "Dx1", "Dx2", "Dy1", 
        "Dy2")
    return(out.df)
    invisible()
}
