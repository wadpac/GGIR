library(GGIR)
context("ggetbout")
test_that("g.getbout produces expected output", {
  # Metric 1
  set.seed(300)
  bm1 = g.getbout(x=round(runif(1000,0.4,1)),boutduration = 120,boutcriter=0.9,
                  closedbout=FALSE,bout.metric=1,ws3=5)
  bm1Ex = c(rep(0,42), rep(1,6), 0, rep(1,10), 0, rep(1,13), 0, rep(1,4),
            0, rep(1,18), 0, rep(1,6), 0, rep(1,8), 0, 1, 0, rep(1,3),
            0, rep(1,14), 0, rep(1,7), 0, rep(1,8), 0, rep(1,16), 0,
            rep(1,4), rep(0,11), 1, 1, 0, rep(1,8), 0, rep(1,8), 0, rep(1,27),
            0, rep(1,12), 0, 1, 1, 0, rep(1,7), 0, 0, 1, 1,
            0, rep(1,5), 0, 1, 1, 0, rep(1,6), 0, rep(1,28), 0, 1, rep(0,693))
  bm1Eboutcount = c(rep(0,42), rep(1,131), rep(0,11), rep(1,123), rep(0,693))
  expect_that(sum(bm1$x),equals(228))
  expect_that(sum(bm1$boutcount),equals(254))
  expect_that(bm1$x,equals(bm1Ex))
  expect_that(bm1$boutcount,equals(bm1Eboutcount))
  
  # Metric 2
  set.seed(300)
  bm2 = g.getbout(x=round(runif(1000,0.4,1)),boutduration = 120,boutcriter=0.9,
                  closedbout=FALSE,bout.metric=2,ws3=5)
  
  bm2Ex = c(rep(0,42), rep(1,286), rep(0,672))
  bm2Eboutcount = c(rep(0,42), rep(1,286), rep(0,672))
  
  expect_that(sum(bm2$x),equals(286))
  expect_that(sum(bm2$boutcount),equals(286))
  expect_that(bm2$x,equals(bm2Ex))
  expect_that(bm2$boutcount,equals(bm2Eboutcount))
  
  # Metric 3
  set.seed(100)
  bm3 = g.getbout(x=round(runif(1000,0.4,1)),boutduration = 120,boutcriter=0.9,
                  closedbout=FALSE,bout.metric=3,ws3=5)
  bm3Ex = c(0, 0, 0, 0, rep(1,124), rep(0,91), rep(1,208), rep(0,573))
  bm3Eboutcount = c( 0, 0, 0, 0, rep(1,124),rep(0,91), rep(1,208), rep(0,573))
  expect_that(sum(bm3$x),equals(332))
  expect_that(bm3$x,equals(bm3Ex))
  expect_that(bm3$boutcount,equals(bm3Eboutcount))
  
  # Metric 4
  set.seed(300)
  bm4 = g.getbout(x=round(runif(1000,0.4,1)),boutduration = 120,boutcriter=0.9,
                  closedbout=FALSE,bout.metric=4,ws3=5)
  bm4Ex = c(rep(0,42), rep(1,285), rep(0,673))
  bm4Eboutcount = c(rep(0,42), rep(1,285), rep(0,673))
  expect_that(sum(bm4$x),equals(285))
  expect_that(sum(bm4$boutcount),equals(285))
  expect_that(bm4$x,equals(bm4Ex))
  expect_that(bm4$boutcount,equals(bm4Eboutcount))
  
  # Metric 5
  set.seed(300)
  bm5 = g.getbout(x=round(runif(1000,0.4,1)),boutduration = 120,boutcriter=0.9,
                  closedbout=FALSE,bout.metric=5,ws3=5)
  bm5Ex = c(rep(0,42), rep(1,285), rep(0,673))
  bm5Eboutcount = c(rep(0,42), rep(1,285), rep(0,673))
  expect_that(sum(bm5$x),equals(285))
  expect_that(sum(bm5$boutcount),equals(285))
  
  # -------------
  # Metric 6 - function to create fake bouts of a given duration and boutcrit
  # in this way, we can predict the time in bouts that should be detected
  # -------------
  dummybout = function(totaldur_min = 60,
                       ws3 = 5,
                       boutdur = 10,
                       boutdur_detect = 10,
                       boutcriter = 0.8,
                       nBouts = 1) {
    x = rep(0, totaldur_min*60/ws3) # initialize vector fo 1 hour
    boutdur = boutdur * 60/ws3
    boutdur_detect = boutdur_detect * 60/ws3
    gaps = boutdur_detect * (1-boutcriter)
    bout = rep(1, boutdur)
    # random position for gaps without gaps > 1min
    gap1min = 60/ws3 + 1
    longest_gap = gap1min + 1
    while (longest_gap >= gap1min) {
      zeroes = sort(sample(x = 2:(length(bout) - 1), size = gaps))
      temp <- cumsum(c(1, diff(zeroes) - 1))
      temp2 <- rle(temp)
      longest_gap_tmp = zeroes[which(temp == with(temp2, values[which.max(lengths)]))]
      longest_gap = length(longest_gap_tmp)
      # if longest_gap <= 1min, then the random zeroes generated are fine and this breaks the loop
    }
    bout[zeroes] = 0
    # random position in x to place the bouts
    sep_between_bouts = boutdur + gap1min #bouts need to be separated at least for more than 1 min
    sep = sep_between_bouts - 1
    while (sep < sep_between_bouts) {
      where = sort(sample(x = 1:(length(x) - boutdur), size = nBouts))
      if (length(where) < 2) sep = sep_between_bouts + 1 # if only one bout, any position is good
      if (length(where) >= 2) {
        sep_bouts = diff(where)
        sep = min(sep_bouts)
      }
    }
    # impute bouts in x
    for (start_i in where) x[start_i:(start_i + boutdur - 1)] = bout
    
    return(x)
  }
  
  # -------------
  # Metric 6 - random tests with variable configurations
  # -------------
  for (i in 1:500) {
    # epoch lenght will be of 5, 15, or 60 seconds
    ws3 = c(5, 15, 60)[sample(x = 1:3, size = 1)]
    
    # duration of the bout to be created with dummybout()
    # from 5 to 30 min in 5-min increments
    boutdur = c(5, 10, 15, 20, 25, 30)[sample(x = 1:6, size = 1)]
    
    # boutdur in minutes to detect bouts with g.getbout 
    # (this should always be lower than boutdur)
    boutdur_detect = 60
    while (boutdur_detect > boutdur) {
      boutdur_detect = c(5, 10, 15, 25, 30)[sample(x = 1:5, size = 1)]
    }
    
    # boutcriter, one of 0.8, 0.9, 1
    boutcriter = c(0.8, 0.9, 1)[sample(x = 1:3, size = 1)]
    
    # number of bouts to be created
    nBouts = c(1, 2, 3)[sample(x = 1:3, size = 1)]
    
    # dummy bout to test the function, we will run a vector of 24 hours with 
    # randomly allocated nBouts
    x = dummybout(totaldur_min = 1440, ws3 = ws3, boutdur = boutdur, boutdur_detect = boutdur_detect, 
                  boutcriter = boutcriter, nBouts = nBouts)
    
    # bout detection with g.getbout
    out = g.getbout(x = x, boutduration = boutdur_detect*60/ws3, boutcriter = boutcriter, 
                    closedbout = FALSE, bout.metric = 6, ws3 = ws3)
    
    # expected time in bout to be detected by g.getbout
    expected_bout = (boutdur*60/ws3)*nBouts
    
    # test
    expect_equal(sum(out$x == 1), expected_bout)
  }
})