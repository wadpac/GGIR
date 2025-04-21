library(GGIR)
context("checkMilestoneFolders")
test_that("checkMilestoneFolders creates expected folders and gives expected warnings", {
  test_folder = "test_milestone_folders"
  dir.create(test_folder)
  expect_error(checkMilestoneFolders(test_folder, partNumber = 5))
  expect_true(dir.exists(paste0(test_folder, "/meta/ms5.out")))
  if (dir.exists(test_folder)) unlink(test_folder, recursive = TRUE)
})