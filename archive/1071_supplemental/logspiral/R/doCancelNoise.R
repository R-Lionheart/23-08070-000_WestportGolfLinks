doCancelNoise <-
function (x = x, y = y, spiral.output = spiral.output, vertical=TRUE,
                       plot.it = TRUE,     gtitle = "") 
{

if(vertical) 
{
spiral.dev <- spiral.output$v.deviations
noise.target <-  spiral.output$parameters2["sd.vmsd"]
}
else
{
spiral.dev <- spiral.output$deviations
noise.target <- spiral.output$parameters2["sd.msd"]
}
#
    arc <- spiral.output$arc.length
    best.spar <- bestSmooth(x, y, target = noise.target)
    smoothed <- arcSmooth2D(x, y, smooth.factor = best.spar)
    smooth.dev <- smoothed$v.deviation
# has the outline been flipped to fit the spiral(s), if so, flip deviations
# used 'flip?' and 'flipped' so spiral output NOT yet consistent
    if (spiral.output$parameters3[3]) 
        smooth.dev <- -1 * smooth.dev
    no.noise <- spiral.dev - smooth.dev
    output <- data.frame(x, y, arc, spiral.dev, smooth.dev, no.noise)
dev.set(5)
    if (plot.it) {
        plot(arc, no.noise, type = "l", xlab = "Arc length", 
            ylab = "Noise cancelled deviation", col = 4)
        axis(4, labels = F)
        axis(3, labels = F)
        abline(h = 0, col = 2)
        title(gtitle)
    }
    return(output)
}
