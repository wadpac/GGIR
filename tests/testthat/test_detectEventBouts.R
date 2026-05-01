library(GGIR)
context("detectEventBouts")
test_that("Detect bouts in events", {
  skip_on_cran()
  
  # create dummy test data
  metashort = data.frame(ENMO = c(rep(0, 24), rep(0.12, 42), rep(0, 24)),
                         step_count = c(rep(0, 24), rep(5, 42), rep(0, 24)))
  cn_metashort = colnames(metashort)
  anwindices = 1:84
  anwi_index = 1
  varnum = metashort$step_count[anwindices]
  varnum_event = rep(0, length(varnum))
  varnum_event[which(metashort$ENMO[anwindices] > 0.05)] = 10
  ws3 = 5
  anwi_nameindices = "_1234hrs"
  daysummary = matrix("", 1, 50)
  fi = 1
  di = 1
  ds_names = ""
  myfun = list(ilevels = c(0, 50, 100),
               clevels = c(0, 30, 50),
               qlevels = c(0.25, 0.5, 0.75),
               ebout.dur = c(1, 5, 10),
               ebout.th.cad = c(30, 100),
               ebout.th.acc = c(50, 100),
               ebout.criter = 1,
               ebout.condition = "AND")

  # run function  
  eventBouts = detectEventBouts(myfun, varnum_event = varnum_event,
                                varnum = varnum, 
                                UnitReScale = 1000,
                                daysummary = daysummary,
                                ds_names = ds_names,
                                di = di, fi = fi,
                                ws3 = ws3,
                                boutnameEnding = "_anyRandomText")
  
  
  # all bouts of 1 minute have the same estimates as they all meet the cadence and acc criteria
  expect_equal(as.numeric(eventBouts$daysummary)[1:12], c(rep(c(3.5, 1.0, 3.5), 4)))
  
  # expect different estimates are calculated
  expect_true("ExtFunEvent_number_B5M100%_cad30ANDacc50__anyRandomText" %in% eventBouts$ds_names)
  expect_true("ExtFunEvent_totdur_B1M100%_cad30ANDacc100__anyRandomText" %in% eventBouts$ds_names)
  expect_true("ExtFunEvent_totdur_B1M100%_cad100ANDacc100__anyRandomText" %in% eventBouts$ds_names)
  expect_true("ExtFunEvent_totdur_B1M100%_cad100ANDacc50__anyRandomText" %in% eventBouts$ds_names)
  
  # Bouts of 5 or 10 minutes are undetected with any cadence and acc criteria
  expect_equal(sum(as.numeric(eventBouts$daysummary)[13:36]), 0)
  
})
