test_that(".onAttach handles an unavailable CRAN package index", {
  cran_index_requested <- FALSE
  behind_cran <- FALSE
  no_internet <- function(...) {
    cran_index_requested <<- TRUE
    matrix(
      character(),
      nrow = 0,
      ncol = 2,
      dimnames = list(NULL, c("Package", "Version"))
    )
  }

  on_attach <- GGIR:::.onAttach
  environment(on_attach) <- list2env(
    list(
      interactive = function() TRUE,
      available.packages = no_internet,
      packageStartupMessage = function(...) behind_cran <<- TRUE
    ),
    parent = environment(GGIR:::.onAttach)
  )

  old_options <- options(repos = c(CRAN = "https://cran.example.invalid"))
  on.exit(options(old_options), add = TRUE)

  expect_no_error(on_attach())
  expect_true(cran_index_requested)
  expect_false(behind_cran)
})
