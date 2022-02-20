round_decimals_df = function(df = c(), digits = 3) {
  # function to convert character to numeric if possible
  as.numeric.ifpossible = function(x) {
    out = tryCatch(
      {
        as.numeric(as.character(x))
      },
      error = function(cond) {
        return(x)
      },
      warning = function(cond) {
        # message(cond)
        # Choose a return value in case of warning
        return(x)
      }
    )
    return(out)
  }
  
  # main code
  for (i in 1:ncol(df)) {
    # convert to numeric
    df[,i] = as.numeric.ifpossible(x = df[, i])
    # if is numeric, round
    if (is.numeric(df[, i])) df[, i] = round(x = df[, i], digits = digits)
  }
  return(df)
}