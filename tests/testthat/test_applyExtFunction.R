library(GGIR)
context("applyExtFunction")
test_that("Function is applied to acceleration data results in expected output", {
  skip_on_cran()
  sf = 40
  S = rep(c(1,5,7,9),each=sf)
  data = as.matrix(data.frame(x=S, y=S, z=S))
  ws3 = 1
  # External function
  exampleExtFunction = function(data=c(), parameters=c()) {
    output = data * parameters[1]
    data = data.frame(data, agglevel=rep(1:4,each=30))
    output = aggregate(data,by=list(data$agglevel),FUN=mean)
    output = output[,-c(1,ncol(output))]
    return(output)
  }
  
  myfun =  list(FUN = exampleExtFunction,
                parameters = 1.1,
                expected_sample_rate =  30, # resample data to 30 Hertz before applying function
                expected_unit = "mg",
                minlength = 1,
                outputres = 1,
                colnames = c("A","B","C"),
                outputtype = "numeric", #"numeric" (averaging is possible), "category" (majority vote)
                aggfunction = mean,
                timestamp = as.numeric(Sys.time())) # for unit test only
  
  # Note: If function applyExtFunction is used directly then object myfun 
  # cannnot carry a logical value because the timestamp can only be added 
  # in the g.getmeta function from which applyExtFunction is called. However,
  # you can provide a numeric value to indicate start time in seconds since
  # 1-1-1970 If the user nonetheless still tries to supply a logical value
  # then applyExtFunction overwrites this.
  
  output = applyExtFunction(data, myfun, sf, ws3)
  
  expect_that(ncol(output),equals(4))
  expect_that(nrow(output),equals(4))
  expect_that(sum(output[,2:4]),equals(66000))
  expect_that(output[3,2],equals(7000))
  
  # test with different settings ----
  ws3 = 2 # to force aggregation in output
  myfun =  list(FUN = exampleExtFunction,
                parameters = 1.1,
                expected_sample_rate = 30, # resample data to 30 Hertz before applying function
                expected_unit = "ms2",
                minlength = 1,
                outputres = 1,
                colnames = c("A","B","C"),
                outputtype = "numeric", #"numeric" (averaging is possible), "category" (majority vote)
                aggfunction = mean,
                timestamp = TRUE) # for unit test only
  
  expect_warning({ # warning from setting timestamp = TRUE
    output = applyExtFunction(data, myfun, sf, ws3)
  })
  
  expect_that(ncol(output),equals(3))
  expect_that(nrow(output),equals(2)) # 2 rows because they were aggregated to match ws3
  expect_that(sum(output[,2:3]),equals(215.82))
  
  # test with different settings ----
  myfun =  list(FUN = exampleExtFunction,
                parameters = 1.1,
                expected_sample_rate = 30, # resample data to 30 Hertz before applying function
                expected_unit = "ms2",
                minlength = 1,
                outputres = 4,
                colnames = c("A","B","C"),
                outputtype = "category", #"numeric" (averaging is possible), "category" (majority vote)
                aggfunction = mean,
                timestamp = TRUE) # for unit test only
  
  expect_warning({ # warning from setting timestamp = TRUE
    output = applyExtFunction(data, myfun, sf, ws3)
  })
  
  expect_that(ncol(output),equals(3))
  expect_that(nrow(output),equals(4))
  expect_that(sum(output[,2:3]),equals(431.64))
  
  # test check_myfun warnings and errors
    expect_error(check_myfun(myfun = 4, ws3),
                 regexp = "not a list")
    expect_error(check_myfun(myfun = list(FUN = mean, 
                                          unexpected = "element"), ws3),
                 regexp = "unexpected elements")
    expect_error(check_myfun(myfun = list(FUN = mean, 
                                          parameters = 4), ws3),
                 regexp = "misses the following elements")
    expect_error(check_myfun(myfun = list(FUN = "not_a_function",
                                          parameters = 1.1,
                                          expected_sample_rate = 30, # resample data to 30 Hertz before applying function
                                          expected_unit = "ms2",
                                          minlength = 1,
                                          outputres = 4,
                                          colnames = c("A","B","C"),
                                          outputtype = "category", #"numeric" (averaging is possible), "category" (majority vote)
                                          aggfunction = mean,
                                          timestamp = Sys.time()), ws3),
                 regexp = "FUN in myfun is not a function")
    expect_error(check_myfun(myfun = list(FUN = exampleExtFunction,
                                          parameters = 1.1,
                                          expected_sample_rate = "Sixty", # resample data to 30 Hertz before applying function
                                          expected_unit = "ms2",
                                          minlength = 1,
                                          outputres = 4,
                                          colnames = c("A","B","C"),
                                          outputtype = "category", #"numeric" (averaging is possible), "category" (majority vote)
                                          aggfunction = mean,
                                          timestamp = Sys.time()), ws3),
                 regexp = "expected_sample_rate in myfun is not numeric")
    expect_error(check_myfun(myfun = list(FUN = exampleExtFunction,
                                          parameters = 1.1,
                                          expected_sample_rate = 30, # resample data to 30 Hertz before applying function
                                          expected_unit = "meters",
                                          minlength = 1,
                                          outputres = 4,
                                          colnames = c("A","B","C"),
                                          outputtype = "category", #"numeric" (averaging is possible), "category" (majority vote)
                                          aggfunction = mean,
                                          timestamp = Sys.time()), ws3),
                 regexp = "lacks a clear specification of the expected_unit")
    expect_error(check_myfun(myfun = list(FUN = exampleExtFunction,
                                          parameters = 1.1,
                                          expected_sample_rate = 30, # resample data to 30 Hertz before applying function
                                          expected_unit = "ms2",
                                          minlength = 1,
                                          outputres = 4,
                                          colnames = 1:3,
                                          outputtype = "category", #"numeric" (averaging is possible), "category" (majority vote)
                                          aggfunction = mean,
                                          timestamp = Sys.time()), ws3),
                 regexp = "colnames in myfun does not hold a character value")
    expect_error(check_myfun(myfun = list(FUN = exampleExtFunction,
                                          parameters = 1.1,
                                          expected_sample_rate = 30, # resample data to 30 Hertz before applying function
                                          expected_unit = "ms2",
                                          minlength = 1:4,
                                          outputres = 4,
                                          colnames = c("X","Y","Z"),
                                          outputtype = "category", #"numeric" (averaging is possible), "category" (majority vote)
                                          aggfunction = mean,
                                          timestamp = Sys.time()), ws3),
                 regexp = "minlength in myfun does not have one value")
    expect_error(check_myfun(myfun = list(FUN = exampleExtFunction,
                                          parameters = 1.1,
                                          expected_sample_rate = 30, # resample data to 30 Hertz before applying function
                                          expected_unit = "ms2",
                                          minlength = "one",
                                          outputres = 4,
                                          colnames = c("X","Y","Z"),
                                          outputtype = "category", #"numeric" (averaging is possible), "category" (majority vote)
                                          aggfunction = mean,
                                          timestamp = Sys.time()), ws3),
                 regexp = "minlength in myfun is not numeric")
    expect_error(check_myfun(myfun = list(FUN = exampleExtFunction,
                                          parameters = 1.1,
                                          expected_sample_rate = 30, # resample data to 30 Hertz before applying function
                                          expected_unit = "ms2",
                                          minlength = 1,
                                          outputres = 4:10,
                                          colnames = c("X","Y","Z"),
                                          outputtype = "category", #"numeric" (averaging is possible), "category" (majority vote)
                                          aggfunction = mean,
                                          timestamp = Sys.time()), ws3),
                 regexp = "outputres in myfun does not have one value")
    expect_error(check_myfun(myfun = list(FUN = exampleExtFunction,
                                          parameters = 1.1,
                                          expected_sample_rate = 30, # resample data to 30 Hertz before applying function
                                          expected_unit = "ms2",
                                          minlength = 1,
                                          outputres = "four",
                                          colnames = c("X","Y","Z"),
                                          outputtype = "category", #"numeric" (averaging is possible), "category" (majority vote)
                                          aggfunction = mean,
                                          timestamp = Sys.time()), ws3),
                 regexp = "outputres in myfun is not numeric")
    expect_error(check_myfun(myfun = list(FUN = exampleExtFunction,
                                          parameters = 1.1,
                                          expected_sample_rate = 30, # resample data to 30 Hertz before applying function
                                          expected_unit = "ms2",
                                          minlength = 1,
                                          outputres = 4.4,
                                          colnames = c("X","Y","Z"),
                                          outputtype = "category", #"numeric" (averaging is possible), "category" (majority vote)
                                          aggfunction = mean,
                                          timestamp = Sys.time()), ws3),
                 regexp = "outputres in myfun should be a round number")
    expect_error(check_myfun(myfun = list(FUN = exampleExtFunction,
                                          parameters = 1.1,
                                          expected_sample_rate = 30, # resample data to 30 Hertz before applying function
                                          expected_unit = "ms2",
                                          minlength = 1,
                                          outputres = 4,
                                          colnames = c("X","Y","Z"),
                                          outputtype = 2, #"numeric" (averaging is possible), "category" (majority vote)
                                          aggfunction = mean,
                                          timestamp = Sys.time()), ws3),
                 regexp = "outputtype is expected to be a character")
    expect_error(check_myfun(myfun = list(FUN = exampleExtFunction,
                                          parameters = 1.1,
                                          expected_sample_rate = 30, # resample data to 30 Hertz before applying function
                                          expected_unit = "ms2",
                                          minlength = 1,
                                          outputres = 4,
                                          colnames = c("X","Y","Z"),
                                          outputtype = "numeric", #"numeric" (averaging is possible), "category" (majority vote)
                                          aggfunction = "mean",
                                          timestamp = Sys.time()), ws3),
                 regexp = "Element aggfunction is not a function object")
    
    
})
