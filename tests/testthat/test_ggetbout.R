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
    bouts1 = g.getbout(x = xtest, boutduration = boutduration, boutcriter = boutcriter, ws3 = ws3)
    expect_equal(sum(bouts1), 10 * N)
    expect_equal(bouts1, c(rep(0, 22 * N), rep(1, 10 * N), rep(0, 20 * N)))
    expect_equal(bouts1, c(rep(0, 22 * N), rep(1, 10 * N), rep(0, 20 * N)))
    
    # Metric 7 - ignore gaps when boutcriter is 1
    xtest = x = c(rep(1, 4 * N), rep(0, 3 * N), rep(1, 6 * N),rep(0, 3 * N), rep(1, 10 * N), rep(0, 1 * N) , rep(1, 9 * N))
    bouts2 = g.getbout(x = xtest, boutduration = boutduration, boutcriter = boutcriter, ws3 = ws3)
    expect_equal(sum(bouts2), 10 * N)
    expect_equal(sum(bouts2), 10 * N)
    
    # Metric 7 - ability to detect odd length bouts
    boutduration = 9 * N
    xtest = x = c(rep(0, 20 * N), rep(1, 9 * N), rep(0, 20 * N))
    bouts3 = g.getbout(x = xtest, boutduration = boutduration, boutcriter = boutcriter, ws3 = ws3)
    expect_equal(sum(bouts3), 9 * N)
    
    # Metric 7 - ability to detect bouts with grace periods
    xtest = c(rep(0, 15 * N), rep(1,6 * N), rep(0, 1 * N), rep(1,10 * N), rep(0,20 * N))
    boutcriter = 0.9
    boutduration = 10 * N
    bouts4 = g.getbout(x = xtest, boutduration = boutduration, boutcriter = boutcriter, ws3 = ws3)
    expect_equal(sum(bouts4), 17 * N)
    
    # Metric 7 - ability to detect bouts with grace periods, but also ignore too many grace periods
    xtest = c(rep(0, 15 * N), rep(1, 6 * N), rep(0, 1 * N), rep(1,3 * N), rep(0, 1 * N), rep(1, 4 * N), rep(0, 20 * N))
    bouts5 = g.getbout(x = xtest, boutduration = boutduration, boutcriter = boutcriter, ws3 = ws3)
    expect_equal(sum(bouts5), 10 * N)
    
    
    # Metric 7 - bout length epquals epoch length
    boutduration = 1
    xtest = c(rep(0, 10 * N), 1, rep(0, 10 * N))
    bouts1 = g.getbout(x = xtest, boutduration = boutduration, boutcriter = boutcriter, ws3 = ws3)
    expect_equal(sum(bouts1), 1)
    expect_equal(bouts1, xtest)
    expect_equal(bouts1, xtest)
  }
  
  
  
  
})