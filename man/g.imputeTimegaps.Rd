\name{g.imputeTimegaps}
\alias{g.imputeTimegaps}
\title{
  Impute gaps in three axis raw accelerometer data
}
\description{
  Removes all sample with a zero in each of the three axes, and then imputes time 
  gaps by the last recorded value per axis normalised to 1 _g_
}
\usage{
  g.imputeTimegaps(x, xyzCol, timeCol, sf, k=0.25)
}
\arguments{
  \item{x}{
    Data.frame with raw accelerometer data, and a timestamp column with millisecond resolution.
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
}
\value{
  Data.frame based on input x with timegaps inputed

}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
