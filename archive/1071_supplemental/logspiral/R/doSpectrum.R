doSpectrum <-
function(Series, W=1, wave1=NULL, wave2=NULL,  Order = 2., Title = NA, output=FALSE)
{

## Three functions defined that are usually external to doSpectrum
#  Warning: assumes deviations are equally spaced as in a time series.
# best is 
Periodogram <- function(X)
{
#       build periodogram of array, return result as timeseries
nval <- length(X)
nval2 <- (nval + 1.) - trunc((nval + 1.)/2.)
FT <- (fft(X - mean(X)))[1.:nval2]
ts((abs(FT)^2.)/nval, start = 0.)
}

Smooth3 <- function(X, N, FilterLength = 10., FilterType = 3.)
{
##      Suitable for periodgrams where we have deleted the reflected bit
##      N is length of original series
nv2 <- length(X)
if(nv2 != N - trunc((N - 1.)/2.))
stop("lengths inconsistent")
XX <- append(X, rev(X[2.:(N + 1. - nv2)]))
Y <- (Smooth1(XX, FilterLength, FilterType))[1.:nv2]
if(is.ts(X))
Y <- ts(Y, start = start(X), end = end(X), frequency = frequency(X))
Y
}
#
Smooth1 <- function(X, FilterLength = 10., FilterType = 3.)
{
nv2 <- length(X)
k2 <- FilterLength/sqrt(FilterType)
k1 <- 2. * floor((k2 + 1.)/2.) - 1.
w2 <- (k2 - k1)/2.
w1 <- 1. - w2
k2 <- k1 + 2.
w1 <- w1/k1
w2 <- w2/k2
c <- ((0.:(nv2 - 1.)) * 3.141592654)/nv2
c[2.:nv2] <- ((w1 * sin(c[2.:nv2] * k1) + w2 * sin(c[2.:nv2] * k2))/sin(c[2.:nv2]))^FilterType
c[1.] <- 1.
Y <- Re(fft(fft(X) * c, inverse = T))/nv2
if(is.ts(X))
Y <- ts(Y, start = start(X), end = end(X), frequency = frequency(X))
Y
}
##
## main part of function begins here
#
P <- Periodogram(Series)
N <- length(Series)
cat( "Series assumed to have equal increments","\n")
cat( "Series length (N):  ", N, "\n")
N2 <- (N + 1.) - trunc((N + 1.)/2.)
if(is.null(wave1))
wave1 <- 0
if(is.null(wave2))
wave2 <- N2
if (wave2 > N2) stop( "wave2 must be less than half series length")
AR <- ar(Series, aic = F, order.max = Order)
Coef <- AR$ar
cat("ar.coefs : ", Coef, "\n")
Var.pred <- AR$var.pred
cat("residual sd : ", sqrt(Var.pred), "\n")
Q <- complex(imaginary = ((0.:N2) * 2. * pi)/N)
Denom <- rep(complex(real = 1., imaginary = 0.), length(Q))
for(i in 1.:Order)
Denom[1.:length(Q)] <- Denom - Coef[i] * exp(i * Q)
Q <- Var.pred/abs(Denom)^2.
Q <- ts(Q, start = 0.)
Var <- mean(P)
ci <- (2. * Var)/sqrt(W)
S <- Smooth3(P, N, W, 3.)
# BEWARE:  Series starts at ZERO
S <- as.numeric(S)[(wave1 + 1):(wave2 + 1)]
Q <- as.numeric(Q)[ (wave1+1): (wave2 + 1)]
#
Max <- max(S, Var + ci, na.rm = T)
Min <- min(S, Var - ci, na.rm = T)
time.range <- wave1:wave2
plot(time.range, S, ylim = c(Min, Max),type="l", las = 1., 
xlab="Wavenumber", ylab="Power (variation units)")
points(time.range, S, pch=16, cex=0.5)
lines(time.range, Q, col = 4)
lines(c(1., N), rep(Var + ci, 2.), lty = 2.)
lines(c(1., N), rep(Var - ci, 2.), lty = 2.)
axis(4,labels=F)
axis(3,labels=F)
if(is.na(Title))
Title <- paste("Spectrum and AR model:", deparse(substitute(Series)))
title(Title, cex.main=0.9, col.main=4)
if (output)  return(S) 
}
