verifyL3Change <-
function (x = x, y = y, m = c(NULL, NULL), pp = c(-2, -1, 0, 
    1, 2), axis.start = c(NULL, NULL)) 
{
    if (is.null(m)) 
        stop("Must include TWO locations for spiral changes")
    if (!is.null(axis.start)) {
        u <- axis.start[1]
        v <- axis.start[2]
        s.fit <- fitAnyLinear(x = x, y = y, m = m, axis.start = c(u, 
            v), print.input = F)
    }
    else s.fit <- fitAnyLinear(x = x, y = y, m = m)
    xu <- s.fit$parameters1[1]
    yv <- s.fit$parameters1[2]
    m1 <- m[1]
    m2 <- m[2]
    py <- pp
    px <- rev(pp)
    delta.length <- length(pp)
    rss.mat <- matrix(data = NA, ncol = delta.length, nrow = delta.length)
    for (ii in seq(along = px)) {
        for (jj in seq(along = py)) {
            rss.mat[ii, jj] <- as.numeric(unlist(fitAnyLinear(x = x, 
                y = y, m = c(m1 + px[ii], m2 + py[jj]), axis.start = c(xu, 
                  yv), print.input = F)$parameters2[1]))
        }
    }
    rownames(rss.mat) <- as.character(px + m1)
    colnames(rss.mat) <- as.character(py + m2)
    cat( "minium value = ", min(rss.mat), "\n")
    cat("minimum at location in output matrix = ", which(rss.mat == 
        min(rss.mat), arr.ind = T), "\n")
    return(rss.mat)
}
