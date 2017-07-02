library(GGIR)
test_that("g.part2", {
  createtestfile()
  fn = "testfile.csv"
  g.part1(datadir=fn,outputdir=getwd(),f0=1,f1=1,overwrite=TRUE,
                     studyname="test",do.enmo = TRUE,do.anglez=TRUE,do.cal = TRUE)
  g.part2(datadir=fn,metadatadir=paste0(getwd(),"/output_test"),f0=1,f1=1,
          strategy = 1,overwrite=TRUE, hrs.del.start = 0,hrs.del.end = 0,
          maxdur = 2, includedaycrit = 1)
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_that(dir.exists(dirname),is_true())
  expect_that(nrow(IMP$metashort),equals(23940))
  expect_that(round(mean(IMP$metashort$ENMO),digits=5),equals(0.01907))
  expect_that(round(as.numeric(SUM$summary$meas_dur_dys),digits=5),equals(1.38542))
  expect_that(round(as.numeric(SUM$summary$`p10_ENMO_mg_0-24h`),digits=4),equals(2.3456))
  expect_that(round(as.numeric(SUM$summary$`WD_mean_ENMO_mg_24hr`),digits=4),equals(19.4651))
  dn = "output_test"
  if (file.exists(dn))  unlink(dn,recursive=TRUE)
  if (file.exists(fn)) file.remove(fn)
})