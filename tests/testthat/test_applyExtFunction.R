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
  
  myfun =  list(FUN=exampleExtFunction,
                parameters = 1.1,
                expected_sample_rate= 30, # resample data to 30 Hertz before applying function
                expected_unit="mg",
                minlength = 1,
                outputres = 1,
                colnames=c("A","B","C"),
                outputtype="numeric", #"numeric" (averaging is possible), "category" (majority vote)
                aggfunction = mean,
                timestamp=as.numeric(Sys.time())) # for unit test only
  
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
})
