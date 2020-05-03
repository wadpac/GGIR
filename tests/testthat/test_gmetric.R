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
  data = cbind(Gx,Gy,Gz)
  lb = 1; hb = 4.5; TW = 100; n = 4; sf=10
  T1 = g.metric(data,n=n,sf=sf,ii=1,TW=TW,lb=lb,hb=hb,gravity = 1) # HFEN
  T3 = g.metric(data,n=n,sf=sf,ii=3,TW=TW,lb=lb,hb=hb,gravity = 1) # EN
  T5 = g.metric(data,n=n,sf=sf,ii=5,TW=TW,lb=lb,hb=hb,gravity = 1) # HFENplus
  T7 = g.metric(data,n=n,sf=sf,ii=7,TW=TW,lb=lb,hb=hb,gravity = 1) # BFEN
  T9 = g.metric(data,n=n,sf=sf,ii=9,TW=TW,lb=lb,hb=hb,gravity = 1) # LFENMO
  T11 = g.metric(data,n=n,sf=sf,ii=11,TW=TW,lb=lb,hb=hb,gravity = 1) # angles
  T12 = g.metric(data,n=n,sf=sf,ii=12,TW=TW,lb=lb,hb=hb,gravity = 1) # AENMO
  T13 = g.metric(data,n=n,sf=sf,ii=13,TW=TW,lb=lb,hb=hb,gravity = 1) # rolling median (roll_med_acc)
  T14 = g.metric(data,n=n,sf=sf,ii=14,TW=TW,lb=lb,hb=hb,gravity = 1) # direction specific acceleration (dev_roll_med_acc)
  expect_equal(floor(sum(T1)),2136)
  expect_equal(floor(sum(T3)),2819)
  expect_equal(floor(sum(T5)),2518)
  expect_equal(floor(sum(T7)),1993)
  expect_equal(floor(sum(T9)),1216)
  expect_equal(floor(sum(T11)),109218)
  expect_equal(floor(sum(T13)),1477)
  expect_equal(floor(sum(T14)),3531)
  metrics2do = data.frame(do.bfen=T,do.enmo=T,do.lfenmo=T,do.en=T,do.hfen=T,
                          do.hfenplus=T,do.mad=T,do.anglex=F,do.angley=F,
                          do.anglez=F,do.roll_med_acc_x=T,do.roll_med_acc_y=T,do.roll_med_acc_z=T,
                          do.dev_roll_med_acc_x=T,do.dev_roll_med_acc_y=T,
                          do.dev_roll_med_acc_z=T,do.enmoa=T,
                          do.lfx=FALSE, do.lfy=FALSE, do.lfz=FALSE, 
                          do.hfx=FALSE, do.hfy=FALSE, do.hfz=FALSE, 
                          do.bfx=FALSE, do.bfy=FALSE, do.bfz=FALSE,
                          stringsAsFactors = TRUE)
  HH = g.applymetrics(data, n=n, sf=sf, ws3=1 , metrics2do)
  expect_equal(round(sum(HH$BFEN),digits=1), 208.3)
  expect_equal(round(sum(HH$MAD),digits=1), 89.1)
  expect_equal(round(sum(HH$LFENMO),digits=1), 117.9)
  expect_equal(round(sum(HH$HFEN),digits=1), 234.2)
  expect_equal(round(sum(HH$EN),digits=1), 282.0)
  expect_equal(round(sum(HH$HFENplus),digits=1), 345.7)
  expect_equal(round(sum(HH$roll_med_acc_x),digits=1), 162.7)
  expect_equal(round(sum(HH$roll_med_acc_y),digits=1),-9)
  expect_equal(round(sum(HH$roll_med_acc_z),digits=1),-5.9)
  
  expect_equal(round(sum(HH$dev_roll_med_acc_x),digits=1), 118.9)
  expect_equal(round(sum(HH$dev_roll_med_acc_y),digits=1), 117.6)
  expect_equal(round(sum(HH$dev_roll_med_acc_z),digits=1), 116.6)
  expect_equal(round(sum(HH$ENMOa),digits=1),142.0)
})