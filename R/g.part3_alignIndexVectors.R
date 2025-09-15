g.part3_alignIndexVectors = function(x, y, a, b, N) {
  # where N is length of time series
  # and x, y, a and b are index vectors
  # Assumptions:
  # - x and y are same length
  # - for all values y > x
  # This function ensures that:
  # - length of a and b match length of x and y
  # - for all values x < a < b < y
  #-------------------------------------
  # start of a is missing
  if (a[1] > y[1]) {
    a =  c(1, a)
  }
  # start of b is missing
  if (b[1] > y[1]) {
    b =  c(1, b)
  }
  #-------------------------------------
  # end of a is missing and end of y equals N
  if (a[length(a)] < x[length(x)] && y[length(y)] == N) {
    a =  c(a, N)
  }
  # end of b is missing and end of y equals N
  if (b[length(b)] < x[length(x)] && y[length(y)] == N) {
    b =  c(b, N)
  }
  #-------------------------------------
  # end of a is larger than end of y
  if (a[length(a)] > y[length(y)]) {
    a =  a[1:(length(a) - 1)]
  }
  # end of b is larger than end of y
  if (b[length(b)] > y[length(y)]) {
    b =  b[1:(length(b) - 1)]
  }
  #-------------------------------------
  # a is longer than y and x
  if (length(a) > length(y) &&
      length(a) > length(x)) {
    a =  a[1:(length(a) - 1)]
  }
  # b is longer than y and x
  if (length(b) > length(y) &&
      length(b) > length(x)) {
    b =  b[1:(length(b) - 1)]
  }
  # #-------------------------------------
  # # a is longer than y but the end of y 
  # if (length(a) > length(y) &&
  #     a[length(a)] == y[length(y)]) {
  #   a =  a[1:(length(a) - 1)]
  # }
  # if (length(b) > length(y) &&
  #     b[length(b)] == y[length(y)]) {
  #   b =  b[1:(length(b) - 1)]
  # }
  # 
  invisible(list(
    x = x,
    y = y,
    a = a,
    b = b
  ))
}