\name{createtestfile}
\alias{createtestfile}
\title{
Creates csv data file for testing purposes
}
\description{
Creates file in the Actigraph csv data format with dummy data
that can be used for testing. The file includes accelerometer data 
with bouts of higher acceleration, variations non-movement periods
in a range of accelerometer positions to allow for testing the
auto-calibration functionality.}
\usage{
  createtestfile(sf=3,Nmin=2000,storagelocation=c())
}
\arguments{
  \item{sf}{
    Sample frequency in Hertz, the default here is low to minimize file size
  }
  \item{Nmin}{
    Number of minutes (minimum is 2000)
  }
  \item{storagelocation}{
    Location where the test file named testfile.csv will be stored
    If no value is provided then the function uses the current 
    working directory
  }
}
\value{
 The function does not produce any output values. Only the file is
 stored
}
  
\examples{
  \dontrun{
    createtestfile()
  }
}
