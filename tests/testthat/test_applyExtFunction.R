library(GGIR)
context("applyExtFunction")
test_that("Function is applied to acceleration data results in expected output", {
  skip_on_cran()
  sf = 40
  S = rep(c(1,5,7,9), each = sf)
  data = as.matrix(data.frame(x = S, y = S, z = S))
  ws3 = 1
  # External function
  exampleExtFunction = function(data=c(), parameters=c()) {
    output = data * parameters[1]
    data = data.frame(data, agglevel = rep(1:4,each = 30))
    output = aggregate(data,by = list(data$agglevel),FUN = mean)
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
  output = output$OutputExternalFunction
  
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
  output = output$OutputExternalFunction
  
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
                outputtype = "numeric", 
                aggfunction = mean,
                timestamp = TRUE) # for unit test only
  
  expect_warning({ # warning from setting timestamp = TRUE
    output = applyExtFunction(data, myfun, sf, ws3)
  })
  output = output$OutputExternalFunction
  
  expect_that(ncol(output),equals(3))
  expect_that(nrow(output),equals(8))
  expect_that(output[, 2],
              equals(rep(c(9.81, 49.05, 68.67, 88.29), each = 2)))

  # Returned timestamps follow the same resolution correction as output values
  timestampExtFunction = function(data = c(), parameters = c()) {
    data.frame(
      timestamp = parameters$timestamps,
      value = parameters$values
    )
  }

  timestamp_data = matrix(
    1,
    nrow = 8,
    ncol = 3,
    dimnames = list(NULL, c("x", "y", "z"))
  )

  timestamp_myfun = list(
    FUN = timestampExtFunction,
    parameters = list(
      timestamps = 100:103,
      values = 1:4
    ),
    expected_sample_rate = 1,
    expected_unit = "g",
    minlength = 1,
    outputres = 1,
    colnames = "value",
    outputtype = "numeric",
    aggfunction = mean,
    timestamp = 100
  )

  timestamp_output = applyExtFunction(
    timestamp_data,
    timestamp_myfun,
    sf = 1,
    ws3 = 2
  )$OutputExternalFunction

  expect_equal(timestamp_output$timestamp, c(100, 102))
  expect_equal(timestamp_output$value, c(1.5, 3.5))

  timestamp_myfun$parameters = list(
    timestamps = c(100, 104),
    values = c(1, 2)
  )
  timestamp_myfun$outputres = 4

  timestamp_output = applyExtFunction(
    timestamp_data,
    timestamp_myfun,
    sf = 1,
    ws3 = 2
  )$OutputExternalFunction

  expect_equal(timestamp_output$timestamp, c(100, 102, 104, 106))
  expect_equal(timestamp_output$value, c(1, 1, 2, 2))
  
  # Character vector output keeps its labels and levels
  data3 = matrix(1, nrow = 8, ncol = 3, dimnames = list(NULL, c("x", "y", "z")))
  chr_myfun = list(FUN = function(data, parameters) c("sit", "walk", "sit", "run"),
                   parameters = NULL, expected_sample_rate = 1, expected_unit = "g",
                   minlength = 1, outputres = 2, colnames = "activity",
                   outputtype = "character", aggfunction = function(x) x[1],
                   reporttype = "type")
  chr_output = applyExtFunction(data3, chr_myfun, sf = 1, ws3 = 2)
  expect_equal(as.character(chr_output$OutputExternalFunction$activity),
               c("sit", "walk", "sit", "run"))
  expect_equal(chr_output$LevelsExternalFunction[[1]], c("run", "sit", "walk"))
  
  # Mixed numeric and character matrix output, aggregated to ws3
  mixed_myfun = chr_myfun
  mixed_myfun$FUN = function(data, parameters) {
    cbind(num = c(1.5, 2.5, 3.5, 4.5), cat = c("sit", "walk", "sit", "run"))
  }
  mixed_myfun$colnames = c("num", "cat")
  mixed_myfun$outputtype = c("numeric", "character")
  mixed_myfun$aggfunction = list(mean, function(x) x[1])
  mixed_myfun$reporttype = c("scalar", "type")
  mixed_output = applyExtFunction(data3, mixed_myfun, sf = 1, ws3 = 4)
  expect_equal(mixed_output$OutputExternalFunction$num, c(2, 4))
  expect_equal(as.character(mixed_output$OutputExternalFunction$cat), c("sit", "sit"))
  expect_length(mixed_output$LevelsExternalFunction, 2)
  expect_null(mixed_output$LevelsExternalFunction[[1]])

  # Aggregation retains epochs whose external output is entirely missing
  missing_myfun = chr_myfun
  missing_myfun$FUN = function(data, parameters) c(NA_real_, NA_real_, 3, 4)
  missing_myfun$colnames = "value"
  missing_myfun$outputtype = "numeric"
  missing_myfun$aggfunction = mean
  missing_myfun$reporttype = NULL
  missing_myfun$outputres = 1
  missing_output = applyExtFunction(data3, missing_myfun, sf = 1, ws3 = 2)
  expect_equal(nrow(missing_output$OutputExternalFunction), 2)
  expect_equal(missing_output$OutputExternalFunction$value, c(NA_real_, 3.5))
  
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
                                        colnames = c(),
                                        outputtype = "category", #"numeric" (averaging is possible), "category" (majority vote)
                                        aggfunction = mean,
                                        timestamp = Sys.time()), ws3),
               regexp = "Element colnames in myfun does not have a value")
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
  duplicate_names_myfun = chr_myfun
  duplicate_names_myfun$colnames = c("value", "value")
  expect_error(
    check_myfun(duplicate_names_myfun, windowsizes = 2),
    regexp = "colnames in myfun must contain unique values"
  )
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
                                        outputres = 5,
                                        colnames = c("X","Y","Z"),
                                        outputtype = "numeric", #"numeric" (averaging is possible), "category" (majority vote)
                                        aggfunction = mean,
                                        timestamp = Sys.time()), ws3),
               regexp = "are not a multitude of each other")
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
               regexp = "Element aggfunction should be a function or a list of functions.")
  
  
})
