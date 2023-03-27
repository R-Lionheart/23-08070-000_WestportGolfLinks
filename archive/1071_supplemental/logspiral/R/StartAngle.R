StartAngle <-
function (xc, yc, u1, v1) 
{
    start.theta <- atan(abs((yc - v1))/abs(xc - u1))
    if (xc - u1 < 0 && yc - v1 > 0) 
        start.theta <- pi - start.theta
    else if (xc - u1 < 0 && yc - v1 < 0) 
        start.theta <- pi + start.theta
    else if (xc - u1 > 0 && yc - v1 < 0) 
        start.theta <- 2 * pi - start.theta
    return(start.theta)
}
