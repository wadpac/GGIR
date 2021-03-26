\name{separategravity}
\alias{separategravity}
\title{
  Separate gravity from acceleration signal.
}
\description{
  Separates gravitational acceleration from the acceleration signal with the
  help of gyrocope data. Output is a orientation of gravity and acceleration,
  both within the local (sensor) coordinate system.
}
\usage{
  separategravity(acc, gyr, sf)
}
\arguments{
  \item{acc}{
    Three column matrix or data.frame with the x, y, z acceleration in g-units
  }
  \item{gyr}{
    Three column matrix or data.frame with the x, y, z angule velocity
    in radians per second
  }
  \item{sf}{
    Sample frequency in Hertz
  }
  
}
\value{
Output of the function is list containing the following objects:\cr
\itemize{
\item acclocal = 3 column matrix with x, y, and z acceleration in g-units
\item gvector = 3 column matrix with x, y, and z coordinates of gravity vector
}
}
\author{
Vincent T van Hees <v.vanhees@accelting.com>
}