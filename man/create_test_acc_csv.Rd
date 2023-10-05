\name{create_test_acc_csv}
\alias{create_test_acc_csv}
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
  create_test_acc_csv(sf=3,Nmin=2000,storagelocation=c(),
                      start_time = NULL, starts_at_midnight = FALSE)
}
\arguments{
  \item{sf}{
    Sample frequency in Hertz, the default here is low to minimize file size
  }
  \item{Nmin}{
    Number of minutes (minimum is 720)
  }
  \item{storagelocation}{
    Location where the test file named testfile.csv will be stored
    If no value is provided then the function uses the current 
    working directory
  }
  \item{start_time}{
    Start time of the recording, in the hh:mm:ss format.
  }
  \item{starts_at_midnight}{
    Boolean indicating whether the recording should start at 
    midnight. Ignored if start_time is specified.
  }
}
\value{
 The function does not produce any output values. Only the file is
 stored
}
  
\examples{
  \dontrun{
    create_test_acc_csv()
  }
}
