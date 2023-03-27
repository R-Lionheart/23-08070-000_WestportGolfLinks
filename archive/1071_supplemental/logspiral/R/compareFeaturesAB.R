compareFeaturesAB <-
function( spiralA = spiralA, spiralB=spiralB, print.it=FALSE)
{
# based on 10 Wavelet and 20 Fourier variances
    test.waveA <- spiralA[1:10]
    test.waveB <- spiralB[1:10]
#
            wratio.diff <- -1* abs(test.waveA - test.waveB)
    wratio.diff.sum <- sum(wratio.diff)
#
#
    test.specA <- spiralA[11:30]
    test.specB <- spiralB[11:30]
    fdiff <- -1*abs(test.specA - test.specB)
    fdiff.sum <- sum(fdiff)/1000

#
 
    if (print.it) {
        cat("WAVELET distance between spirals A and B", "\n")
        print(wratio.diff.sum)
        cat("FOURIER distance between spirals A and B", "\n")
        print(fdiff.sum)
}
}
