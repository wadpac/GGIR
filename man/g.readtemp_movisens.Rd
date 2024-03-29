\name{g.readtemp_movisens}
\alias{g.readtemp_movisens}
\title{
Reads the temperature from movisens files.
}
\description{
  Reads the temperature from movisens files, resamples it and adds 
  it to the matrix where accelerations are stored
}
\usage{
g.readtemp_movisens(datafile, from = c(), to = c(), acc_sf, acc_length,
                    interpolationType=1)
}
\arguments{
  \item{datafile}{
    Full path to the folder where the movisens bin files are stored. Note that 
    movisens store a set of bin file in one folder per recording. GGIR will read 
    the pertinent bin file to access to the temperature data.
  }
  \item{from}{
    Origin point to derive the temperature from movisens files (automatically 
    calculated by GGIR)
  }
  \item{to}{
    End point to derive the temperature from movisens files (automatically 
    calculated by GGIR)
  }
  \item{acc_sf}{
    Sample frequency of acceleration data
  }
  \item{acc_length}{
    number of acceleration data samples
  }
  \item{interpolationType}{
    Integer to indicate type of interpolation to be used when resampling time series (mainly relevant for Axivity sensors), 1=linear, 2=nearest neighbour.
  }
}
\value{
  Data matrix with the temperature values resampled at 64 Hz.
}
\keyword{internal}
\examples{
\dontrun{
  P = g.readtemp_movisens(datafile, from = c(), to = c(), acc_sf = 64, acc_length = 3000)
}
}
