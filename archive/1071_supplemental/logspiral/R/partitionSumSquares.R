partitionSumSquares <-
function (spiral.input=spiral.input, plot.it = T, print.output=T) 
{
    requireNamespace("logspiral")
   # radial not vertical deviations
    sdev <- spiral.input$deviations
    arc <- spiral.input$arc.length
sdev.n <- length(sdev)
arc.max <- max(arc)
sdev.var <- var(sdev)*(sdev.n -1)
    #
    xx.df <- approx(arc, sdev, rule = 2, xout = seq(0, arc.max, length = sdev.n))
    sdev.interp <- xx.df$y 
    sdev.spec <- fft(sdev.interp - mean(sdev.interp))
sdev.spec <- (Mod(sdev.spec)^2)/length(sdev)
n2  <- (sdev.n + 1) - trunc( (sdev.n + 1)/2)
var.fft <- 2*sum(sdev.spec[1:n2]) - sdev.spec[n2]
sum.squares <- c( 2*sdev.spec[1:(n2-1)], sdev.spec[n2] )
#
if(print.output)
{
cat( "Warning: Fourier based on equal spacing of arc length","\n")
cat("Fourier & raw  sum squares  , N", c(var.fft,sdev.var, sdev.n), "\n")
}
    if (plot.it) {
        dev.set(2)
        plot(arc, sdev, xlab = "Arc length", ylab = "Spiral deviation", type = "s", col = 3)
title("Deviations with equal spaced arc length")
#
#  NOTE : doubling of variance because plotting half the neg. & pos. frequencies
#
dev.set(4)
        plot(sdev.spec[1:20]*2, type = "b", pch = 15, xlab = "Frequency", 
            col = 4, ylab = "Power (variance)", main = "Fourier spectrum detail ")
dev.set(5)
        plot(sdev.spec[1:n2]*2, type = "b", pch = 15, xlab = "Frequency", 
            col = 4, ylab = "Power (variance)", main = "Fourier spectrum variance ")
    }
    output <- c( sum.squares)
   return(output)
}
