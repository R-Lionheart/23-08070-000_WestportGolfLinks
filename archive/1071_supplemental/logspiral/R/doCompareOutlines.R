doCompareOutlines <-
function( a.df,  b.df , plot.deviations=TRUE , print.area=TRUE)
{ 
# a.df is the reference outline (best if equal spaced) with 
#         two columns having the names x, and y.
# b.df is the outline to compare ( spaced the same as outlline a.df)
#
# a outline (best as a data frame)
x1 <- a.df$x
y1 <- a.df$y
arc.length <- cumsum( c(0, sqrt( diff(x1)^2 + diff(y1)^2)))
# b outline ( best as a data frame)
x2 <- b.df$x
y2 <- b.df$y
#
# assumed outlines are already aligned 
# (if not use translation and rotation as in procPCA in shapes package))
# 
# distance from b to a using formula in Maxwell (1958) page 81
A <- diff(y1)
B <- -1*diff(x1)
C <- diff(x1)*y1[-1] -1*diff(y1)*x1[-1]
#
dist.pts <- (A*x2[-1] + B*y2[-1] + C)/sqrt( A^2 + B^2)
dist.start <- (A[1]*x2[1] + B[1]*y2[1] + C[1])/sqrt( A[1]^2 + B[1]^2)
dist.pts <- c(dist.start, dist.pts)
dist.pts <- -1*dist.pts
if(plot.deviations)
{
plot( arc.length, dist.pts, type="b", pch=16, cex=0.6,
xlab="Arc length of reference outline", ylab="Difference between outlines")
abline(h=0, col=4,lwd=1.5)
axis(4,labels=F)
par(new=T)
plot( seq(1,length(dist.pts),1), dist.pts,type="n", xlab="", ylab="",axes=F)
axis(3)
}
## area under curve of deviations
area.ab <- sum( diff(arc.length)*( head( abs(dist.pts),-1) +
              tail( abs(dist.pts ), -1)))/2
if (print.area) cat("Area under deviations =  ", area.ab,"\n")
output <- data.frame( x1,y1, x2,y2,arc=arc.length, deviation = dist.pts )
return(output)
}
