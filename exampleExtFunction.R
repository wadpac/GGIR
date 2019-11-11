exampleExtFunction = function(data=c(), model_coefficients=c()) {
  print(summary(data))
  print(model_coefficients)
  output = mean(data) * model_coefficients[1] + mean(data) * model_coefficients[2] + mean(data) * model_coefficients[3]
  print(output)
  return(output)
}