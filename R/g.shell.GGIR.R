g.shell.GGIR = function(...) {
  if (runif(n = 1, min = 0, max = 1) > 0.95) {
    message("\n[Hint] Did you know that instead of function g.shell.GGIR(...)",
            " you can also type GGIR(...). We will keep both options",
            " but we thought you may like to know the shorter version.",
            " Technically speaking g.shell.GGIR is a very short function",
            " that passes its arguments on to",
            " function GGIR from the GGIR package.\n")
  }
  .args <- as.list(match.call()[-1])
  do.call(GGIR, .args)
}
