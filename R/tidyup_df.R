tidyup_df = function(df = c(), digits = 3) {
  
  
  myRoundFun = function(x) {
    tryCatch(round(as.numeric(as.character(x)), digits = digits), 
             error = function(cond) return(x),
             warning = function(cond) return(x))
  }
  
  df = df[rowSums(!is.na(df)) > 0, ]
  if (ncol(df) > 1) {
    # Round columns that can be coerced to numeric
    # But round fragmentation metrics to 6 decimal places instead of default 3
    # because some fragmentation metrics can reveal relevant info
    # at 4 or 5 decimal places
    for (fragCols in c(TRUE, FALSE)) {
      if (fragCols == TRUE) {
        colsID = grep(pattern = "FRAG_", x = colnames(df), invert = TRUE)
        digitsRound = 6
      } else {
        colsID = grep(pattern = "FRAG_", x = colnames(df), invert = FALSE)
        digitsRound = digits
      }
      if (length(colsID) > 0) {
        df[, colsID] = lapply(df[, colsID], myRoundFun)
      }
    }
    # list to data frame
    df = as.data.frame(df)
  }
  return(df)
}