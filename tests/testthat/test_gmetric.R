library(GGIR)
context("g.metric")
test_that("g.getmetric calculates the correct metric values", {
  N = 1500
  set.seed(300)
  Gx = rnorm(n = N, mean = 1, sd = 1)
  set.seed(400)
  Gy = rnorm(n = N, mean = 0, sd = 1)
  set.seed(500)
  Gz = rnorm(n = N, mean = 0, sd = 1)
  lb = 1; hb = 4.5; TW = 100; n = 4
  T1 = g.metric(Gx,Gy,Gz,n=n,sf=10,ii=1,TW=TW,lb=lb,hb=hb,gravity = 1) # HFEN
  T3 = g.metric(Gx,Gy,Gz,n=n,sf=10,ii=3,TW=TW,lb=lb,hb=hb,gravity = 1) # EN
  T5 = g.metric(Gx,Gy,Gz,n=n,sf=10,ii=5,TW=TW,lb=lb,hb=hb,gravity = 1) # HFENplus
  T7 = g.metric(Gx,Gy,Gz,n=n,sf=10,ii=7,TW=TW,lb=lb,hb=hb,gravity = 1) # BFEN
  T9 = g.metric(Gx,Gy,Gz,n=n,sf=10,ii=9,TW=TW,lb=lb,hb=hb,gravity = 1) # LFENMO
  T11 = g.metric(Gx,Gy,Gz,n=n,sf=10,ii=11,TW=TW,lb=lb,hb=hb,gravity = 1) # angles
  T12 = g.metric(Gx,Gy,Gz,n=n,sf=10,ii=12,TW=TW,lb=lb,hb=hb,gravity = 1) # AENMO
  T13 = g.metric(Gx,Gy,Gz,n=n,sf=10,ii=13,TW=TW,lb=lb,hb=hb,gravity = 1) # rolling median (roll_med_acc)
  T14 = g.metric(Gx,Gy,Gz,n=n,sf=10,ii=14,TW=TW,lb=lb,hb=hb,gravity = 1) # direction specific acceleration (dev_roll_med_acc)
  expect_equal(floor(sum(T1)),2136)
  expect_equal(floor(sum(T3)),2819)
  expect_equal(floor(sum(T5)),2518)
  expect_equal(floor(sum(T7)),1993)
  expect_equal(floor(sum(T9)),1216)
  expect_equal(floor(sum(T11)),109218)
  expect_equal(floor(sum(T13)),1477)
  expect_equal(floor(sum(T14)),3531)

})