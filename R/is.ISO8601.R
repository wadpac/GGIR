is.ISO8601 = function(x) {
  is.ISO = FALSE
  if (is.character(x)) {
    NNeg = length(unlist(strsplit(x,"[-]")))
    NPos = length(unlist(strsplit(x,"[+]")))
    if (NPos == 2 | NNeg == 4 | NNeg == 2) {
      is.ISO = TRUE
    }
  }
  return(is.ISO)
}