g.createcoordinates = function(r,timeline) {
  if (length(which(abs(diff(r)) == 1) > 0)) {	
    if (r[1] == 1) {
      x0 = c(timeline[1],timeline[which(diff(r) == 1) + 1])
      x1 = timeline[which(diff(r) == -1) + 1]
      if (r[length(timeline)] == 1) { #file ends with non-wear
        x1 = c(x1,timeline[length(timeline)])
      }
    } else {
      x0 = timeline[which(diff(r) == 1) + 1]
      x1 = timeline[which(diff(r) == -1) + 1]
      if (r[length(timeline)] == 1) { #file ends with non-wear
        x1 = c(x1,timeline[length(timeline)])
      }
    }
  } else {
    x0 = c()
    x1 = c()
  }
  invisible(list(x0=x0,x1=x1))
}