exampleExtFunction = function(data=c(), parameters=c()) {
  # data: matrix where columns are raw acceleration data
  # parameters: In this function the parameters is just the sample rate.
  library("activityCounts")
  mycounts = counts(data=data, hertz=parameters,x_axis=1,y_axis=2,z_axis=3, start_time = Sys.time())
  mycounts = mycounts[,2:4] #do not provide timestamps to GGIR
  return(mycounts)
}