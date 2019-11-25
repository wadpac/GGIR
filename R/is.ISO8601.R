is.ISO8601 = function(x) {
  NNeg = length(unlist(strsplit(x,"[-]")))
  NPos = length(unlist(strsplit(x,"[+]")))
  is.ISO = FALSE
  if (NPos == 2) {
    is.ISO = TRUE
  } else if (NNeg == 4 | NNeg == 2) {
    is.ISO = TRUE
  }
  return(is.ISO)
}