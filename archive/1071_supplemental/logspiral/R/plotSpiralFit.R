plotSpiralFit <-
function (spiral.output = spiral.output, change = c(NULL, NULL, 
    NULL), xdata = xdata, ydata = ydata, title.label = " ", output.device = 5) 
{
    if (is.null(change)) 
        m <- 1
    else m <- length(change) + 1
    dev.set(output.device)
    x.fit <- spiral.output$xpred.orig
    y.fit <- spiral.output$ypred.orig
    xaxis <- spiral.output$parameters1[1]
    yaxis <- spiral.output$parameters1[2]
    requireNamespace("MASS")
    MASS::eqscplot(xdata, ydata, pch = 1, axes = F, xlab = "", 
        ylab = "")
    lines(x.fit, y.fit, col = 2, lwd = 2)
    points(xaxis, yaxis, pch = 10, cex = 2.5, col = 2)
    points(xdata[change], ydata[change], pch = 16, col = 4, cex = 1.8)
    if (m == 1) 
        title(paste(" One Spiral", " for specimen ", title.label), 
            cex.main = 1.1, col.main = 4, adj = 0)
    else if (m == 2) 
        title(paste(" Two Spirals change at ", change[1], " for ", 
            title.label), cex.main = 1, col.main = 4, adj = 0)
    else if (m == 3) 
        title(paste(" Three Spirals change at ", change[1], ",", 
            change[2], " for ", title.label), cex.main = 1, col.main = 4, 
            adj = 0)
    else if (m == 4) 
        title(paste(" Four Spirals change at ", change[1], ",", 
            change[2], ",", change[3], " for ", title.label), 
            cex.main = 1, col.main = 4, adj = 0)
}
