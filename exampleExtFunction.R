exampleExtFunction = function(data=c(), model_coefficients=c(), sf) {
  # print(summary(data))
  print(model_coefficients)
  output = mean(data) * model_coefficients[1] + mean(data) * model_coefficients[2] + mean(data) * model_coefficients[3]
  print(output)
  print(dim(data))
  
  library("activityCounts")
  mycounts = counts(data=data, hertz=model_coefficients,x_axis=1,y_axis=2,z_axis=3)
  print(summary(mycounts))
  
  
  return(output)
}