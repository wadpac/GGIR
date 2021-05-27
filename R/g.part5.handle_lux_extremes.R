g.part5.handle_lux_extremes = function(lux) {
  # detect sequence of extreme lux values
  detect.pattern = function(patrn, x) {
    # credits to Berend Hasselman for following three lines of code
    # https://r.789695.n4.nabble.com/matching-a-sequence-in-a-vector-tp4389523p4389909.html
    patrn.rev = rev(patrn)
    w = embed(x,length(patrn))
    w.pos <- which(apply(w,1,function(r) all(r == patrn.rev)))
    if (length(w.pos) > 0) {
      result = w.pos:(w.pos+length(patrn)-1)
    } else {
      result = c()
    }
    return(result)
  }
  extremes_lux_values = which(lux > 120000)
  correction_log = rep(0, length(lux))
  correction_log[extremes_lux_values] = 1
  # remove sequences lasting 3 long epochs or longer (default 45 minutes)
  extreme_sequence = detect.pattern(patrn = rep(1, 3), x = correction_log)
  if (length(extreme_sequence) > 0) { 
    lux[extreme_sequence] = NA
    correction_log[extreme_sequence] = 2
  }
  # for remaining extremes impute by average of neighbours
  extremes = which(correction_log == 1)
  if (length(extremes) > 0) {
    lux[extremes] = NA
    for (eb in 1:length(extremes)) {
      if (extremes[eb] == 1) {
        impluxi = 1:2
      } else if (extremes[eb] > 1 & extremes[eb] < length(lux)) {
        impluxi = (extremes[eb]-1):(extremes[eb]+1)
      } else if (extremes[eb] == length(lux)) {
        impluxi = (length(lux)-1):length(lux)
      }
      implux = mean(lux[impluxi], na.rm = TRUE)
      if (length(implux) > 0) lux[extremes[eb]] = implux
    }
  }
  invisible(list(lux=lux, correction_log=correction_log))
}