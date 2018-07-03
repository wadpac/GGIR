getFirstTimestamp <- function(f, p1) {
    hhr <- GENEAread::header.info(f)
    hcal <- attr(hhr, "calibration")
    t1 <- hcal$t1
    rate <- 1 / hcal$freq
    nrows <- (p1 - t1) / rate
    newRate <- 1 / as.numeric(hhr$Value[[2]])
    as.POSIXct(t1 + (nrows * newRate), origin = "1970-1-1")
}

