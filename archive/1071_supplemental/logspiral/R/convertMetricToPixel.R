convertMetricToPixel <-
function(xeqn = c(1,0,0), yeqn=c(0,1,0), x=x, y=y, check.input=FALSE, check.output=FALSE)
{
# Converts metric coordinates to raster coordinates
# for plotting on an image
# xeqn, yeqn are the equation coefficients that converted raster 
# to metric so the metric could be analysed (eg spiral fitting)
#
# Equation coefficients are in order of X (raster), Y (raster) and intercept
# This order matches the output from Vextractor coordinate references
# x, y are the input metric coordinates
# function outputs the converted coordinates as a data frame with xx,yy
# Tony Aldridge 6 Aug. 2019
# some checking on input
#
#
# translation (intercept) coefficients
xk <- xeqn[3]
yk <- yeqn[3]
# transformation matrix that was used to convert raster to metric
rast.metric <- matrix( c(xeqn[1],xeqn[2], yeqn[1], yeqn[2]),byrow=T, nrow=2)
#
if (check.input )
{
cat("Input transformation matrix", "\n")
print(rast.metric)
cat("Intercept constants x,y", "\n")
print(c(xk,yk))
}
# transformation matrix from metric to raster
metric.rast <- solve( rast.metric)
if( check.output )
{
cat("Output transformation matrix", "\n")
print(metric.rast)
}
#
# convert metric (mm) to raster (pixel)
xx <- metric.rast[1,1]*(x - xk) + metric.rast[1,2]*(y - yk)
yy <- metric.rast[2,1]*(x - xk) + metric.rast[2,2]*(y - yk)
output.df <- data.frame(xx,yy)
return(output.df)
}
