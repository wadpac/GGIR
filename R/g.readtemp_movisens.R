g.readtemp_movisens = function(datafile, from = c(), to = c(), acc_sf, acc_length, interpolationType=1) {
    # Acceleration data and temperature were sampled at different rates,
    # so we need to resample temperature to get the same sampling rate
    # as what we have for acceleration.

    temp_sf = 1 # temperature is most likely sampled at 1Hz, but we'll double-check later

    temperature = unisensR::readUnisensSignalEntry(dirname(datafile), "temp.bin")
    temperature = temperature$temp

    origin = unisensR::readUnisensStartTime(dirname(datafile))
    rawTime = seq(origin, origin + length(temperature) - 1, by = 1)

    acc_length = unisensR::getUnisensSignalSampleCount(dirname(datafile), "acc.bin")
    step = (length(temperature) - 1) / acc_length   #ratio of temp sf to acc sf in movisens data
    timeRes = seq(rawTime[1], rawTime[length(rawTime)], step)
    timeRes = timeRes[-length(timeRes)]

    temperature = GGIRread::resample(as.matrix(temperature), rawTime, timeRes, length(temperature), type=interpolationType)

    if(length(from) > 0 & length(to) > 0) {
       temperature = temperature[from:to]
    }
    invisible(temperature)
}
