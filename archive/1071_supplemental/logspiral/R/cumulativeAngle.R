cumulativeAngle <-
function (x, y, u, v) 
{
# angle about axis located at u,v
    xy.r <- sqrt((x - u)^2 + (y - v)^2)
    x.unit <- (x - u)/xy.r
    y.unit <- (y - v)/xy.r
    npts <- length(x.unit)
    theta <- acos(abs(x.unit[1:(npts - 1)] * x.unit[2:npts] + 
        y.unit[1:(npts - 1)] * y.unit[2:npts]))
    theta <- c(0, theta)
    theta <- (theta)
    eps <- 1e-05
    theta.sign <- rep(+99, npts)
    check1 <- rotate2D(x = x.unit[1:(npts - 1)], y = y.unit[1:(npts - 
        1)], theta = -theta[2:npts])
    xc <- c(x.unit[1], check1$x)
    yc <- c(y.unit[1], check1$y)
    check1.x <- x.unit - xc
    check1.y <- y.unit - yc
    theta.sign <- ifelse(check1.x < eps & check1.y < eps, 1,-1)
    theta <- theta * theta.sign
    theta <- cumsum(theta)
    return(theta)
}
