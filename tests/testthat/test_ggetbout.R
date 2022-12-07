library(GGIR)
context("ggetbout")
test_that("g.getbout produces expected output", {
  
  for (ws3 in c(5, 15, 30, 60)) { # epoch size should not influence the result, so we can turn it into a loop
    cat(paste0("\nepochsize ", ws3))
    boutcriter = 1
    N = 60 / ws3 # N is number of epochs in minute
    boutduration = 10 * N
    # Metric 7 - 2 bouts out of which 1 is long enough
    xtest = c(rep(0, 15 * N), rep(1, 6 * N), rep(0, 1 * N), rep(1, 10 * N), rep(0, 20 * N))
    expected_bout_start = ((15 + 6 + 1) * N) + 1
    expected_bout_end = ((15 + 6 + 1 + 10) * N)
    bouts1 = g.getbout(x = xtest, boutduration = boutduration, boutcriter = boutcriter, ws3 = ws3)
    expect_equal(sum(bouts1), 10 * N)
    expect_equal(bouts1, c(rep(0, 22 * N), rep(1, 10 * N), rep(0, 20 * N)))
    expect_equal(bouts1, c(rep(0, 22 * N), rep(1, 10 * N), rep(0, 20 * N)))
    expect_equal(min(which(bouts1 == 1)), expected_bout_start)
    expect_equal(max(which(bouts1 == 1)), expected_bout_end)
    
    # Metric 7 - ignore gaps when boutcriter is 1
    xtest = x = c(rep(1, 4 * N), rep(0, 3 * N), rep(1, 6 * N),rep(0, 3 * N), rep(1, 10 * N), rep(0, 1 * N) , rep(1, 9 * N))
    expected_bout_start = ((4 + 3 + 6 + 3) * N) + 1
    expected_bout_end = ((4 + 3 + 6 + 3 + 10) * N)
    bouts2 = g.getbout(x = xtest, boutduration = boutduration, boutcriter = boutcriter, ws3 = ws3)
    expect_equal(sum(bouts2), 10 * N)
    expect_equal(sum(bouts2), 10 * N)
    expect_equal(min(which(bouts2 == 1)), expected_bout_start)
    expect_equal(max(which(bouts2 == 1)), expected_bout_end)
    
    # Metric 7 - ability to detect odd length bouts
    boutduration = 9 * N
    xtest = x = c(rep(0, 20 * N), rep(1, 9 * N), rep(0, 20 * N))
    expected_bout_start = (20 * N) + 1
    expected_bout_end = ((20 + 9) * N)
    bouts3 = g.getbout(x = xtest, boutduration = boutduration, boutcriter = boutcriter, ws3 = ws3)
    expect_equal(sum(bouts3), 9 * N)
    expect_equal(min(which(bouts3 == 1)), expected_bout_start)
    expect_equal(max(which(bouts3 == 1)), expected_bout_end)
    
    # Metric 7 - ability to detect bouts with grace periods
    xtest = c(rep(0, 15 * N), rep(1,6 * N), rep(0, 1 * N), rep(1,10 * N), rep(0,20 * N))
    expected_bout_start = (15 * N) + 1
    expected_bout_end = ((15 + 6 + 1 + 10) * N)
    boutcriter = 0.9
    boutduration = 10 * N
    bouts4 = g.getbout(x = xtest, boutduration = boutduration, boutcriter = boutcriter, ws3 = ws3)
    expect_equal(sum(bouts4), 17 * N)
    expect_equal(min(which(bouts4 == 1)), expected_bout_start)
    expect_equal(max(which(bouts4 == 1)), expected_bout_end)
    
    # Metric 7 - ability to detect bouts with grace periods, but also ignore too many grace periods
    xtest = c(rep(0, 15 * N), rep(1, 6 * N), rep(0, 1 * N), rep(1,3 * N), rep(0, 1 * N), rep(1, 4 * N), rep(0, 20 * N))
    expected_bout_start = (15 * N) + 1
    expected_bout_end = ((15 + 6 + 1 + 3) * N)
    bouts5 = g.getbout(x = xtest, boutduration = boutduration, boutcriter = boutcriter, ws3 = ws3)
    expect_equal(sum(bouts5), 10 * N)
    expect_equal(min(which(bouts5 == 1)), expected_bout_start)
    expect_equal(max(which(bouts5 == 1)), expected_bout_end)
    
    # Metric 7 - bout length equals epoch length
    boutduration = 1
    xtest = c(rep(0, 10 * N), 1, rep(0, 10 * N))
    bouts6 = g.getbout(x = xtest, boutduration = boutduration, boutcriter = boutcriter, ws3 = ws3)
    expect_equal(sum(bouts6), 1)
    expect_equal(bouts6, xtest)
    expect_equal(bouts6, xtest)
  }
  
  
  
  
})