computeL1DeviationFeatures <-
function (x = x, y = y, plot.it = T) 
{
# Extracts 10 wavelet variances and 20 Fourier variances
#
    requireNamespace("wmtsa")
    requireNamespace("logspiral")
     spiral <- logspiral::fitAnyLinear(x, y, plot.deviations = T, 
        print.input = F, specimen = "Single spiral fit to given outline")
    sdev <- spiral$v.deviations
    arc <- spiral$arc.length
    sdev.scale <- sdev * 10/(max(sdev) - min(sdev))
    arc.scale <- arc * 100/max(arc)
    xx.df <- approx(arc.scale, sdev.scale, rule = 2, xout = seq(0, 
        100, length = 1024))
    sdev.scale.interp <- xx.df$y
    sdev.spec <- fft(sdev.scale.interp - mean(sdev.scale.interp))
    sdev.spec <- ((abs(sdev.spec)^2)/1024)[1:512]
    sdev.modwt <- wmtsa::wavMODWT(sdev.scale.interp - mean(sdev.scale.interp), 
        n.levels = 10)
    sdev.modwt.list <- sdev.modwt$data[-11]
    sdev.var <- as.numeric(unlist(lapply(sdev.modwt.list, var)))
    if (plot.it) {
        dev.set(2)
        plot(xx.df$x, xx.df$y, xlab = "Arc length (1024)", ylab = "Spiral deviation", 
            type = "s", col = 3)
        dev.set(3)
        plot(sdev.var, type = "b", pch = 16, xlab = "Scale (2^n)", 
            ylab = "Variance", main = "Wavelet scalogram of scaled deviations & arc")
        dev.set(5)
        plot(sdev.spec[1:25], type = "b", pch = 15, xlab = "Frequency", 
            col = 4, ylab = "Power", main = "Fourier spectrum of scaled deviations & arc")
    }
    output <- c(sdev.var, sdev.spec[1:20])
    return(output)
}
