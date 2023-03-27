computeDeviationWavelets <-
function (spiral.output=spiral.output, wave.levels = NULL, 
print.var=TRUE, plot.var = TRUE, plot.maxWave=TRUE, units="mm") 
{
#
# object spiral.output must have a column called: deviations
# if plotting wavelet the object must also have column: arc.length
#
deviations <- spiral.output$deviations
N <- length(deviations)
if (is.null(wave.levels) )
{  
# max of 16 levels or 65,536 values
power.two <- 2^seq(1,16) 
wave.levels <- max( which( N/power.two > 1, arr.ind=TRUE) )
}
#
# modwt is maximal overlap discrete wavelet (decomposition)
#
  zmodwt <- wmtsa::wavMODWT( deviations, n.levels=wave.levels)
 #
  zmodwt.lis <- zmodwt$data[ -(wave.levels+1)]
  zvar <- as.numeric( unlist(lapply( zmodwt.lis, var) ))
#
if (print.var)
{
 cat( "variances percent of total:  ",       round((zvar/sum(zvar))*100,2), "\n")  
 cat( "total variance (wavelet, deviations)", sum(zvar),"   ", var(deviations), "\n" )
}
#  
zwave.mat <- matrix(NA, nrow=N, ncol=(wave.levels-1))
for ( ii in seq(1,(wave.levels-1))  )
{
zwave <- wmtsa::wavMRD( zmodwt, level=c(ii) )
zwave <- as.numeric(zwave)
zwave.mat[ , ii] <- zwave
} 
if (plot.var == TRUE)
{ 
dev.set(4)
 plot(wmtsa:: wavVar(deviations, wavelet="s4", n.levels=(wave.levels-2) ) ) # variance plot
}
if (plot.maxWave == TRUE)
{
dev.set(5)
zvar.max <- which( zvar == max(zvar), arr.ind=TRUE)
arc.length <- spiral.output$arc.length
plot( arc.length, deviations, type="s", 
xlab=  paste("Arc length  (",units, ")") ,
ylab=  paste("Spiral deviations (",units,")")    )
lines( arc.length, zwave.mat[, zvar.max] , col=4, lwd=2)
axis(3,labels=FALSE)
axis(4, labels=FALSE)
title( paste( "Max variance MODWT wavelet : ", zvar.max), adj=0, col=4)
}
# output
wavelets <- as.data.frame( zwave.mat)
return(wavelets)
}
