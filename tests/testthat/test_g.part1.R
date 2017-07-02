library(GGIR)
test_that("g.part1", {
  createtestfile()
  fn = "testfile.csv"
  g.part1(datadir=fn,outputdir=getwd(),f0=1,f1=1,
                     studyname="test",do.enmo = TRUE,do.anglez=TRUE,do.cal = TRUE)
  dn = "output_test"
  expect_that(dir.exists(dn),is_true())
  rn = dir("output_test/meta/basic/",full.names = TRUE)
  load(rn[1])
  expect_that(round(C$scale,digits=5),equals(c(0.9852, 0.9852, 0.9852)))
  expect_that(nrow(M$metalong),equals(133))
  expect_that(M$metalong[2,1],equals("2016-06-23T09:15:00+0100"))
  expect_that(nrow(M$metashort),equals(23940))
  expect_that(round(mean(M$metashort$ENMO),digits=5),equals(0.01907))
  expect_that(I$monc,equals(3))
  expect_that(I$sf,equals(3))
  expect_that(I$dformc,equals(2))
  expect_that(C$npoints,equals(2606))
  if (file.exists(dn))  unlink(dn,recursive=TRUE)
  if (file.exists(fn)) file.remove(fn)
})