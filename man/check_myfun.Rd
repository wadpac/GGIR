\name{check_myfun}
\alias{check_myfun}
\title{
  Checks myfun object before it is passed to applyExtfunction
}
\description{
  Checks that object myfun is a list and check the elements of the
  list for: that element names are as expected, that value of each
  element is of the expected type and length.
}
\usage{
  check_myfun(myfun, windowsizes)
  
}
\arguments{
  \item{myfun}{
    See \link{applyExtFunction}
  }
  \item{windowsizes}{
    See \link{g.getmeta}).
  }
}
\value{
  0 if all checkes passed, 1 if one or more checks did not pass. Error
  message are printed to the console with feedback on which checks did 
  not pass.
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}