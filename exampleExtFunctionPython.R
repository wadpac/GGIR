exampleExtFunctionPython = function(data=c(), parameters=c()) {
  # data: matrix where columns are raw acceleration data
  # parameters: In this function the parameters is just the sample rate.
  # # put this in generic R script:
  # library("reticulate")
  # use_virtualenv("~/venv3", required = TRUE) # an local Python environment that has Python 3 and Numpy
  source_python("dominant_frequency.py")
  sf=30 # Assuming 30 hertz as input
  # data = matrix(rexp(3000, rate=.1), ncol=3) # dummy data for testing purposes
  N = nrow(data)
  ws = 5 # windowsize in seconds
  data = data.frame(t= floor(seq(0,(N-1)/sf,by=1/sf)/ws), x=data[,1], y=data[,2], z=data[,3])
  df = aggregate(data, by = list(data$t), FUN=function(x) {return(dominant_frequency(x,sf))}) # now make sure this is apply per X seconds
  df = df[,-c(1:2)]
  return(df)
}