rotate2D <-
function (x, y, theta, u = 0, v = 0) 
{
# theta in radians
    xx <- x - u
    yy <- y - v
    xp <- xx * cos(theta) + yy * sin(theta)
    yp <- -xx * sin(theta) + yy * cos(theta)
    x.df <- data.frame(x = xp + u, y = yp + v)
    return(x.df)
}
