plotSpiralDeviations <-
function (spiral.output = spiral.output, change = c(NULL, NULL, 
    NULL), title.label = "", output.device = 4, units="mm") 
{
    if (is.null(change)) 
        m <- 0
    else m <- length(change) + 1
    dev.set(output.device)
    deviations <- spiral.output$deviations
    arc.length <- spiral.output$arc.length
    plot(arc.length, deviations, type = "s", 
      xlab = paste("Arc length (", units,")" ) , 
        ylab = paste( "Spiral deviations (",units,")")  )
    abline(h = 0, col = 4)
    abline(v = arc.length[change], col = 2, lwd = 2, lty = 3)
    axis(4, labels = F)
    axis(3, labels = F)
    mtext(paste("Spiral deviations : ", title.label), side = 3, 
        outer = T, line = -2, cex = 1.2, col = 4, adj = 0.05)
}
