g.shell.GGIR = function(...) {
  message(paste0("Please note that instead of g.shell.GGIR() you can just write ",
                 "GGIR(), because all g.shell.GGIR() does is forward its input to ",
                 "function GGIR()."), call. = FALSE)
  .args <- as.list(match.call()[-1])
  do.call(GGIR, .args)
}
