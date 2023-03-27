compareFeaturesToCatalogue <-
function (spiral.features = spiral.features, print.it = FALSE, plot.detail = FALSE, 
  output.results=FALSE,  graphic.windows = c(2, 3, 4, 5)) 
{
 catalogue <-   logspiral::catalogueDeviationFeatures 
#
   model.names <- c("L1", "L2", "L3", "L4", "Q1", "C", "QQ", 
        "LQ", "QL", "LQL","L2QL")
    model.breaks <- c(10.5, 88.5, 142.5, 160.5, 216.5, 250.5, 
        316.5, 469.5, 600.5, 744.5)
  model.ordered <- c("L1","L2","LQ","LQL","L2QL","L3","QL","Q", "L4", "C","QQ")
model.seq <- catalogue$type
#
model.seq[model.seq==1.1] <- 1  # L1
model.seq[model.seq==1.2] <- 2  # L2
model.seq[model.seq==3.1] <- 3  # LQ
model.seq[model.seq==3.3] <- 4  # LQL
model.seq[model.seq==3.4] <- 5  # L2QL
model.seq[model.seq==1.3] <- 6  # L3
model.seq[model.seq==3.2] <- 7  # QL
model.seq[model.seq==2.1] <- 8  # Q
model.seq[model.seq==1.4] <- 9  # L4
model.seq[model.seq==2.3] <- 10 # cubic 
model.seq[model.seq==2.4] <- 11  # quartic

#
catalogue <- data.frame(catalogue, model.seq)

    cat.len <- length(catalogue[, 1])
#
# Wavelet, 10 local features : vectorised loop
    test.wave <- spiral.features[1:10]
    wdiff.sum <- NULL
        spiral.wave <- t(as.matrix( catalogue[ , 14:23]))
        wratio.diff <- as.matrix(abs(test.wave - spiral.wave))
        wdiff.sum <- apply(wratio.diff,2,sum)
     zw <- -1 * wdiff.sum
    xcat.df <- data.frame(catalogue[,c( 1:13,44)], zw)
    xxcat.df <- xcat.df[order(xcat.df$zw, decreasing = T), ]
    xxfactor <- as.factor( xxcat.df$type)
    levels(xxfactor) <- model.names
    xxcat.df$type <- xxfactor
#
# Fourier 20 features, more global, vectorised loop
    test.spec <- spiral.features[11:30]
    fdiff.sum <- NULL

        spiral.spec <- t(as.matrix(catalogue[, 24:43]))
        fdiff <- as.matrix(abs(test.spec - spiral.spec))
        fdiff.sum <- apply(fdiff,2,sum)
    
    zf <- -fdiff.sum/1000
    ycat.df <- data.frame( catalogue[,c(1:13,44)], zf)
    yycat.df <- ycat.df[order(ycat.df$zf, decreasing = T), ]
   yyfactor <- as.factor( yycat.df$type)
    levels(yyfactor) <- model.names
    yycat.df$type <- yyfactor
#
# plot the 9 ranked models
    dev.set(graphic.windows[3])
    par(mar = c(5, 5, 5, 5))
    plot(xxcat.df$model.seq[1:9], yycat.df$model.seq[1:9],type = "n", xlim = c(0.5, 11.5), ylim = c(0.5, 11.5),  
        axes = F, xlab = "Local shape features", ylab = " ")
abline(0,1, col=3,lwd=2.5)
points(jitter(xxcat.df$model.seq[1:9], 0.6), jitter(yycat.df$model.seq[1:9], 0.6),
 pch = as.character(seq(1, 9)),type="b", font = 2, cex = 1.2, col=4)
    box(lwd = 1)
    axis(3, at = seq(1, 11), col.ticks = "grey", labels = F)
    axis(1, at = seq(1, 11), col.ticks = "grey", labels = F)
    axis(4, at = seq(1, 11), col.ticks = "grey", labels = F)
    axis(2, at = seq(1, 11), col.ticks = "grey", labels = F)
    text(par("usr")[1] - 0.75, seq(1, 11), labels = model.ordered, 
        font = 2, xpd = TRUE, adj = c(0.9, 0), cex = 0.9)
    text(seq(1, 11,1), par("usr")[3] - 0.75, labels = model.ordered, 
        font = 2, srt = 45, xpd = TRUE, adj = c(0.9, 0), cex = 0.9)
 mtext(side=2, "Global shape features", line=3.5)
    title("Spiral models (ranked) to consider for outline")
#
    if (plot.detail) {
        dev.set(graphic.windows[1])
        plot(1:cat.len, zw, type = "l", xlim=c(0,cat.len+5), xlab = "", ylab = "Wavelet difference", 
            axes = F)
        box(lwd = 1)
        points(xxcat.df$seq[2:6], xxcat.df$zw[2:6], pch = 16, 
            col = 4)
        points(xxcat.df$seq[7:11], xxcat.df$zw[7:11], pch = 1, 
            cex = 1.2, col = 4)
        abline(v = model.breaks, col = 4, lty = 2)
        title("WAVELET scalogram")
        axis(1, at = model.breaks, labels = F, tck = -0.011)
        axis(2, las = 2)
        axis(4, labels = F)
        axis(1, at = c(57, 115, 185, 280, 390, 525, 670,805), labels = c("L2", 
            "L3", "Q", "QQ", "LQ", "QL", "LQL","L2QL"), cex.axis = 0.9, 
            line = -3, lwd = 0, tick = F, font = 2)
        axis(1, at =1, labels = "L1", srt = 90, adj = 0, lwd = 0, 
            lwd.ticks = 0, line = -0.7, font = 2, cex.axis = 1)
        axis(1, at = 148, labels = "L4", srt = 90, adj = 0, lwd = 0, 
            lwd.ticks = 0, line = -0.7, font = 2, cex.axis = 1)
        axis(1, at = 235, labels = "C", srt = 90, adj = 0, lwd = 0, 
            lwd.ticks = 0, line = -0.7, font = 2, cex.axis = 1)
        points(which.max(zw), zw[which.max(zw)], pch = 15, cex = 1.5, 
            col = 2)
        dev.set(graphic.windows[2])
        plot(1:cat.len, zf, type = "l", xlab = "", ylab = "Fourier difference (10^-3)", 
            axes = F,xlim=c(0,cat.len+5))
        box(lwd = 1)
        points(yycat.df$seq[2:6], yycat.df$zf[2:6], pch = 16, 
            col = 4)
        points(yycat.df$seq[7:11], yycat.df$zf[7:11], pch = 1, 
            cex = 1.2, col = 4)
        abline(v = model.breaks, col = 4, lty = 2)
        title("FOURIER spectrum")
        axis(1, at = model.breaks, labels = F, tck = -0.011)
        axis(2, las = 2)
        axis(4, labels = F)
        axis(1, at = c(57, 115, 185, 280, 390, 525, 670,805), labels = c("L2", 
            "L3", "Q", "QQ", "LQ", "QL", "LQL","L2QL"), cex.axis = 0.9, 
            line = -3, lwd = 0, tick = F, font = 2)
        axis(1, at =1, labels = "L1", srt = 90, adj = 0, lwd = 0, tick=F,
            lwd.ticks = 0, line = -0.7, font = 2, cex.axis = 1)
        axis(1, at = 148, labels = "L4", srt = 90, adj = 0, lwd = 0, 
tick=F, lwd.ticks = 0, line = -0.7, font = 2, cex.axis = 1)
        axis(1, at = 235, labels = "C", srt = 90, adj = 0, lwd = 0,
 tick=F,   lwd.ticks = 0, line = -0.7, font = 2, cex.axis = 1)
        points(which.max(zf), zf[which.max(zf)], pch = 15, cex = 1.5, 
            col = 2)
dev.set(5)
# model ratios against ranking on local (blue) and global (black)
old.par <- par("mar")
par(mar=c(5,4,4,4))
 plot(1:9, xxcat.df[1:9 , "zw"], type="b", pch="W", 
       cex=1,col=4, xlim=c(0.5,9.5),
      axes=F,ylab="Local (W = Wavelet) ratio",xlab="")
    box(lwd=1); axis(2)
    axis(1, at=1:9, labels= as.character(xxcat.df[1:9,"type"]), 
           cex.axis=0.9, las=2, font=2,xpd=T,col.axis=4)
par(new=T)
 plot(jitter(1:9), yycat.df[1:9 , "zf"], type="b", pch="F", 
        cex=1.2,col=1, font=2,xlim=c(0.5,9.5),
        axes=F,ylab=" ",xlab=" ")
   box(lwd=1); axis(4, col.axis=1,col.ticks=3)
   axis(3, at=1:9, labels=yycat.df[1:9,"type"], 
        cex.axis=0.9, las=2, col.axis=1,font=2)
   mtext( "Global (F = Fourier) ratio ", col=1, side=4, outer=F, line=2.5)

par(mar=old.par)
    }
    if (print.it) {
        cat("WAVELET top 9 catalogue spiral models to consider", "\\n")
        print(xxcat.df[1:9, ])
        cat("FOURIER top 9 catalogue spiral models to consider", "\\n")
        print(yycat.df[1:9, ])
    }
if (output.results) {
output <- list( local = xxcat.df[1:9,], global= yycat.df[1:9,])
return (output)
}
}
