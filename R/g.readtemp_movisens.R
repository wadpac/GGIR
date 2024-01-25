g.readtemp_movisens = function(datafile, from = c(), to = c(), interpolationType=1) {
    # Acceleration data and temperature were sampled at different rates,
    # so we need to resample temperature to get the same sampling rate
    # as what we have for acceleration.

    temperature = unisensR::readUnisensSignalEntry(dirname(datafile), "temp.bin")
    temperature = temperature$temp

    # we don't care about the exact timestamp values because we'll throw the timestamps away anyway.
    rawTime = seq_len(length(temperature))

    acc_length = unisensR::getUnisensSignalSampleCount(dirname(datafile), "acc.bin")
    timeRes = seq(from = 1, to = rawTime[length(rawTime)], length.out = acc_length)

    temperature = GGIRread::resample(as.matrix(temperature), rawTime, timeRes, length(temperature), type=interpolationType)

    if(length(from) > 0 && length(to) > 0) {
       temperature = temperature[from:to]
    }

    invisible(temperature)
}
