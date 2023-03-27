equalSpace <-
function (x, y, nequal = 50, light.smooth = FALSE) 
{
    z <- as.matrix(cbind(x, y))
    nt <- dim(z)[1]
    if (light.smooth) 
        z <- rbind(z[1, ], (z[-nt, ] + z[-1, ])/2, z[nt, ])
    zdist <- sqrt(diff(z[, 1])^2 + diff(z[, 2])^2)
    t1 <- c(0, cumsum(zdist))/sum(zdist)
    x <- z[, 1]
    y <- z[, 2]
    x <- approx(t1, x, n = nequal)$y
    y <- approx(t1, y, n = nequal)$y
    arc.length <- sqrt(diff(x)^2 + diff(y)^2)
    arc.length <- c(0, cumsum(arc.length))
    return(data.frame(cbind(x, y, arc.length)))
}
