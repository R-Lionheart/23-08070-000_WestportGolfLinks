compareSpiralFits <-
function (s1=NULL, s2=NULL, s3=NULL, s4=NULL, s5=NULL, ratios=TRUE) 
{
   requireNamespace("logspiral")
   # radial not vertical deviations
if(is.null(s1)) stop("Need to input at least one spiral fit")
{
#
sum.squares <- partitionSumSquares( spiral.input = s1, plot.it=F, print.output=F)
n2 <- length(sum.squares)
total.squares <- sum( sum.squares)
# contribution low frequency
total.low <- sum( sum.squares[1:4])
low.f1 <- (total.low/total.squares)*100
#
# contribution to suspected cyclic components
total.cyclic <- sum( sum.squares[5:19])
cyclic.f1 <- (total.cyclic/total.squares)*100
# contribution to high frequency (noise, hopefully)
total.high <-  sum( sum.squares[-c(1:19)])
high.f1 <- ( total.high/total.squares)*100
low.freq <- c(low.f1  )
cyclic.freq <- c(cyclic.f1 )
high.freq <- c(high.f1 )
spiral.name <- deparse(substitute(s1))
total.rss <- total.squares
total.sd <- sqrt( (total.squares/s1$parameters3["df"]))
total.n <- as.numeric(s1$parameters3[1])
total.df <- as.numeric(s1$parameters3[2])
}
if(!is.null(s2))
{
sum.squares <- partitionSumSquares( spiral.input = s2, plot.it=F, print.output=F)
n2 <- length(sum.squares)
total.squares <- sum( sum.squares)
total.low <- c(total.low, sum( sum.squares[1:4]) )
total.cyclic <- c(total.cyclic, sum( sum.squares[5:19]))
total.high <-  c(total.high, sum( sum.squares[-c(1:19)]))
#
low.f2 <- (sum( sum.squares[1:4])/total.squares)*100
cyclic.f2 <- (sum( sum.squares[5:19])/total.squares)*100
high.f2 <- ( sum( sum.squares[-c(1:19)])/total.squares)*100
low.freq <- c(low.freq, low.f2  )
cyclic.freq <- c(cyclic.freq,cyclic.f2)
high.freq <- c(high.freq, high.f2 )
spiral.name <- c(spiral.name,  deparse(substitute(s2)) )
total.rss <- c(total.rss, total.squares)
total.sd <- c(total.sd, sqrt( (total.squares/s2$parameters3["df"])) )
ratio12 <- c(total.low[1]/total.low[2], total.cyclic[1]/total.cyclic[2], total.high[1]/total.high[2])
output.ratio <- matrix(data=ratio12,nrow=1)
total.n <- c(total.n, as.numeric(s2$parameters3[1]))
total.df <- c(total.df, as.numeric(s2$parameters3[2]))

}
#
if(!is.null(s3))
{
sum.squares <- partitionSumSquares( spiral.input = s3, plot.it=F, print.output=F)
n2 <- length(sum.squares)
total.squares <- sum( sum.squares)
total.low <- c(total.low, sum( sum.squares[1:4]) )
total.cyclic <- c(total.cyclic, sum( sum.squares[5:19]))
total.high <-  c(total.high, sum( sum.squares[-c(1:19)]))
low.f3 <- (sum( sum.squares[1:4])/total.squares)*100
cyclic.f3 <- (sum( sum.squares[5:19])/total.squares)*100
high.f3 <- ( sum( sum.squares[-c(1:19)])/total.squares)*100
low.freq <- c(low.freq, low.f3  )
cyclic.freq <- c(cyclic.freq,cyclic.f3)
high.freq <- c(high.freq, high.f3 )
spiral.name <- c(spiral.name,  deparse(substitute(s3)) )
total.rss <- c(total.rss, total.squares)
total.sd <- c(total.sd, sqrt( (total.squares/s3$parameters3["df"])) )
ratio23 <- c(total.low[2]/total.low[3], total.cyclic[2]/total.cyclic[3], total.high[2]/total.high[3])
output.ratio <- rbind(output.ratio, ratio23)
total.n <- c(total.n, as.numeric( s3$parameters3[1]))
total.df <- c(total.df, as.numeric( s3$parameters3[2]))

}
#
if(!is.null(s4))
{
sum.squares <- partitionSumSquares( spiral.input = s4, plot.it=F, print.output=F)
n2 <- length(sum.squares)
total.squares <- sum( sum.squares)
total.low <- c(total.low, sum( sum.squares[1:4]) )
total.cyclic <- c(total.cyclic, sum( sum.squares[5:19]))
total.high <-  c(total.high, sum( sum.squares[-c(1:19)]))
low.f4 <- (sum( sum.squares[1:4])/total.squares)*100
cyclic.f4 <- (sum( sum.squares[5:19])/total.squares)*100
high.f4 <- ( sum( sum.squares[-c(1:19)])/total.squares)*100
low.freq <- c(low.freq, low.f4  )
cyclic.freq <- c(cyclic.freq,cyclic.f4)
high.freq <- c(high.freq, high.f4 )
spiral.name <- c(spiral.name,  deparse(substitute(s4)) )
total.rss <- c(total.rss, total.squares)
total.sd <- c(total.sd, sqrt( (total.squares/s4$parameters3["df"])) )
ratio34 <- c(total.low[3]/total.low[4], total.cyclic[3]/total.cyclic[4], total.high[3]/total.high[4])
output.ratio <- rbind(output.ratio, ratio34)
total.n <- c(total.n, as.numeric(s4$parameters3[1]))
total.df <- c(total.df, as.numeric(s4$parameters3[2]))
}
#
if(!is.null(s5))
{
sum.squares <- partitionSumSquares( spiral.input = s5, plot.it=F, print.output=F)
n2 <- length(sum.squares)
total.squares <- sum( sum.squares)
total.low <- c(total.low, sum( sum.squares[1:4]) )
total.cyclic <- c(total.cyclic, sum( sum.squares[5:19]))
total.high <-  c(total.high, sum( sum.squares[-c(1:19)]))
low.f5 <- (sum( sum.squares[1:4])/total.squares)*100
cyclic.f5 <- (sum( sum.squares[5:19])/total.squares)*100
high.f5 <- ( sum( sum.squares[-c(1:19)])/total.squares)*100
low.freq <- c(low.freq, low.f5  )
cyclic.freq <- c(cyclic.freq,cyclic.f5)
high.freq <- c(high.freq, high.f5 )
spiral.name <- c(spiral.name,  deparse(substitute(s5)) )
total.rss <- c(total.rss, total.squares)
total.sd <- c(total.sd, sqrt( (total.squares/s5$parameters3["df"])) )
ratio45 <- c(total.low[4]/total.low[5], total.cyclic[4]/total.cyclic[5], total.high[4]/total.high[5])
output.ratio <- rbind(output.ratio, ratio45)
total.n <- c(total.n, as.numeric(s5$parameters3[1]))
total.df <- c(total.df, as.numeric(s2$parameters3[2]))
}
#
output.percent <- data.frame(spiral.name, low.freq, cyclic.freq, high.freq,total.rss,total.sd,total.n,total.df)
names(output.percent) <- c("Spiral fit", "LowFreq%","Cyclic%","HighFreq%","RSS total", "Std deviation", "N", "df")
row.names(output.percent)[1] <- "1"
# 
#
if( !is.null(s2) & ratios==TRUE)  # at least two spiral fits have been input so have ratios
 {
output.ratio <- as.data.frame(output.ratio)
names(output.ratio) <- c("LowFreq","Cyclic", "HighFreq")
row.names(output.ratio)[1] <- "ratio12"
output <- list(output.percent, output.ratio)
names(output) <- c("Percent of total variation", "Ratios of variation")
}
else  output <- output.percent 

 return(output)
}
