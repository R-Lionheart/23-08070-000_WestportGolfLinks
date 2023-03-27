doVariogram <-
function(Series, nlags = NULL, plot.it=T, output=F, Title=NA)
{
 #
var <- NULL
std.dev <- NULL
var.n <- NULL
N <- length(Series)
n <- length(Series[Series != "NA"])
mean.x <- mean(Series, na.rm = T)
if(is.null(nlags))
max.lags <- trunc((N + 1)/2)
else max.lags <- nlags
for(i in seq(1, max.lags)) {
vi <- Series[(i + 1):N] - Series[1:(N - i)]
n.lag <- length(vi[vi != "NA"])
var[i] <- sum(vi^2, na.rm = T)/(2 * n.lag * mean.x * mean.x)
std.dev[i] <- sqrt(sum(vi^2, na.rm = T)/(2 * n.lag))
var.n[i] <- n.lag
}
lag.j <- seq(1, max.lags)
if (plot.it) {
    plot(lag.j, std.dev,type="l",xlab="Lag", ylab="Lagged standard deviation")
points(lag.j, std.dev, pch=16,cex=0.5)
axis(3,labels=F)
axis(4,labels=F)
if(is.na(Title))
                Title <- paste("Semi Variogram :", deparse(substitute(Series)))
        title(Title)
}
if (output) {

out <- data.frame(lag = lag.j, variance= var, std.dev = std.dev, n = var.n)
return(out)
}
}
