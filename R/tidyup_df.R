tidyup_df = function(df = c(), digits = 3) {
  df = df[rowSums(!is.na(df)) > 0, ]
  if (ncol(df) > 1) {
    # Round columns that can be coerced to numeric
    df = lapply(df, function(x) tryCatch(round(as.numeric(as.character(x)), digits = digits), 
                                         error = function(cond) return(x),
                                         warning = function(cond) return(x)))
    # list to data frame
    df = as.data.frame(df)
  }
  return(df)
}