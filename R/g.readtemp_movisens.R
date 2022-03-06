g.readtemp_movisens = function(datafile, desiredtz = "", from = c(), to = c(), interpolationType=1) {
    temperature = unisensR::readUnisensSignalEntry(dirname(datafile), "temp.bin")
    temperature = as.data.frame(temperature)
    origin = unisensR::readUnisensStartTime(dirname(datafile))
    temperature$timestamp = seq(origin, origin + nrow(temperature) - 1, by = 1)
    rawTime = vector(mode = "numeric", nrow(temperature))
    rawTime = as.numeric(as.POSIXlt(temperature$timestamp,tz = desiredtz))
    rawTemp = as.matrix(temperature[,-c(which(colnames(temperature) == "timestamp"))])
    acc_length = unisensR::getUnisensSignalSampleCount(dirname(datafile), "acc.bin")
    step = (nrow(temperature) - 1) / acc_length   #ratio of temp sf to acc sf in movisens data
    start = rawTime[1]
    end = rawTime[length(rawTime)]
    timeRes = seq(start, end, step)
    nr = length(timeRes) - 1
    timeRes = as.vector(timeRes[1:nr])
    tempRes = matrix(0,nrow = nr, ncol = ncol(rawTemp), dimnames = list(NULL,colnames(rawTemp)))
    rawLast = nrow(rawTemp)
    tempRes = resample(rawTemp, rawTime, timeRes, rawLast, type=interpolationType) # this is now the resampled temp data
    if(length(from) > 0 & length(to) > 0) {
       temperature = tempRes[from:to]
    } else {
       temperature = tempRes
    }
    invisible(temperature)
}
