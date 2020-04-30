library(GGIR)
context("getfolderstructure")
test_that("getfolderstructure", {
  dn = "A" # name of the datadir folder
  if (file.exists(dn))  unlink(dn,recursive=TRUE)
  dummyfolderA = "A"
  dummyfolderB = "B"
  dummyfolderC = "C"
  dir.create(file.path("A"))
  dir.create(file.path("A/B"))
  dir.create(file.path("A/C"))
  
  write.csv(matrix(0,4,4),file="A/B/testB.csv")
  write.csv(matrix(0,4,4),file="A/C/testC.csv")
  
  folderstructure = getfolderstructure(datadir=dn,referencefnames=c("testB.csv","testC.csv"))
  expect_that(  folderstructure$foldername[1],equals("B"))
  expect_that(  folderstructure$foldername[2],equals("C"))
  if (file.exists(dn))  unlink(dn,recursive=TRUE)
})