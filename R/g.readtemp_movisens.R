g.readtemp_movisens = function(datafile, from = c(), to = c(), acc_sf, acc_length, interpolationType=1) {
    # Acceleration data and temperature were sampled at different rates,
    # so we need to resample temperature to get the same sampling rate
    # as what we have for acceleration.

    temp_sf = 1 # temperature is most likely sampled at 1Hz, but we'll double-check later

    temp_from = ceiling(from / acc_sf * temp_sf)
    temp_to = ceiling(to / acc_sf * temp_sf)

    temperature = unisensR::readUnisensSignalEntry(dirname(datafile), "temp.bin", 
                                                   startIndex = temp_from, endIndex = temp_to)
    new_temp_sf = attr(temperature, "sampleRate")

    # We had guessed that temperature was sampled at 1Hz. Let's check, and if we were wrong, then re-do.
    if (temp_sf != new_temp_sf) {
        temp_from = ceiling(from / acc_sf * new_temp_sf)
        temp_to = ceiling(to / acc_sf * new_temp_sf)

        temperature = unisensR::readUnisensSignalEntry(dirname(datafile), "temp.bin", 
                                                       startIndex = temp_from, endIndex = temp_to)
    }

    temperature = temperature$temp

    # we don't care about the exact timestamp values because we'll throw the timestamps away anyway.
    rawTime = seq_len(length(temperature))
    timeRes = seq(from = 1, to = rawTime[length(rawTime)], length.out = acc_length)

    temperature = GGIRread::resample(as.matrix(temperature), rawTime, timeRes, length(temperature), type=interpolationType)

    invisible(temperature)
}
