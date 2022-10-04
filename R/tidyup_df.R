tidyup_df = function(df = c(), digits = 3) {
  df = df[rowSums(!is.na(df)) > 0, ]
  if (ncol(df) > 1) {
    # browser()
    # is.nan.data.frame <- function(x) {
    #   do.call(cbind, lapply(x, is.nan))
    # }
    # df_bu = df
    # t0 = Sys.time()
    nums = vapply(df, is.numeric, FUN.VALUE = logical(1))
    df[, nums] = round(df[, nums], digits = digits)
    # df[is.na(df) | is.nan.data.frame(df)] = ""
    # print(Sys.time() - t0)
    # df_new = df
    # df = df_bu
    # t0 = Sys.time()
    # for (i in 1:ncol(df)) {
    #   # convert to numeric if possible
    #   df[,i] = tryCatch(as.numeric(as.character(df[, i])),
    #                     error = function(cond) { return(df[, i]) },
    #                     warning = function(cond) { return(df[, i]) } )
    # 
    #   # if numeric, round to n digits
    #   if (is.numeric(df[, i])) df[, i] = round(x = df[, i], digits = digits)

      # replace all NA and NaN values by blank
      # NA_NaN = which(is.na(df[, i]) | is.nan(df[, i]) | df[, i] == "NA" | df[, i] == "NaN")
      # df[NA_NaN, i] = ""
    # }
    # print(Sys.time() - t0)
  }
  return(df)
}
