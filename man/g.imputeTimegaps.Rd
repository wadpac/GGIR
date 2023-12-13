\name{g.imputeTimegaps}
\alias{g.imputeTimegaps}
\title{
  Impute gaps in three axis raw accelerometer data
}
\description{
  Removes all sample with a zero in each of the three axes, and then (as default)
  imputes time  gaps by the last recorded value per axis normalised to 1 _g_
}
\usage{
  g.imputeTimegaps(x, xyzCol, timeCol = c(), sf, k = 0.25, impute = TRUE, 
                   PreviousLastValue = c(0,0,1), 
                   PreviousLastTime = NULL, epochsize = NULL)
}
\arguments{
  \item{x}{
    Data.frame with raw accelerometer data, and a timestamp column with
    millisecond resolution.
  }
  \item{xyzCol}{
    Columnnames or numbers for the x, y and z column
  }
  \item{timeCol}{
    Column name or number for the timestamp column
  }
  \item{sf}{
    Sample frequency in Hertz
  }
   \item{k}{
    Minimum time gap length to be imputed
  }
   \item{impute}{
    Boolean to indicate whether the time gaps identified should be imputed
  }
  \item{PreviousLastValue}{
    Automatically identified last value in previous chunk of data read.
  }
    \item{PreviousLastTime}{
    Automatically identified last timestamp in previous chunk of data read.
  }
  \item{epochsize}{
    Numeric vector of length two, with short and long epoch sizes.
  }
}
\value{
  List including:
  - x, data.frame based on input x with timegaps imputed (as default) or with 
  recordings with 0 values in the three axes removed (if impute = FALSE)
  - QClog, data.frame with information on the number of time gaps found and the 
  total time imputed in minutes
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
