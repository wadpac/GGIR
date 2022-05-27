g.shell.GGIR = function(...) {
  .args <- as.list(match.call()[-1])
  do.call(GGIR, .args)
}
