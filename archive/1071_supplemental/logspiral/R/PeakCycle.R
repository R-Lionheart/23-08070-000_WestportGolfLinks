PeakCycle <-
function (Data = Data, SearchFrac = 0.02) 
{
    requireNamespace("wmtsa")
    Wave <- wmtsa::wavCWT(Data)
    WaveTree <- wmtsa::wavCWTTree(Wave)
    WavePeaks <- wmtsa::wavCWTPeaks(WaveTree, snr.min = 5)
    WavePeaks_Times <- attr(WavePeaks, which = "peaks")[, "iendtime"]
    NewPeakTimes <- c()
    dRange <- round(SearchFrac * length(Data))
    for (i in 1:length(WavePeaks_Times)) {
        NewRange <- max(c(WavePeaks_Times[i] - dRange, 1)):min(c(WavePeaks_Times[i] + 
            dRange, length(Data)))
        NewPeakTimes[i] <- which.max(Data[NewRange]) + NewRange[1] - 
            1
    }
    return(matrix(c(NewPeakTimes, Data[NewPeakTimes]), ncol = 2, 
        dimnames = list(NULL, c("PeakIndices", "Peaks"))))
}
