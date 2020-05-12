\name{g.readtemp_movisens}
\alias{g.readtemp_movisens}
\title{
Reads the temperature from movisens files.
}
\description{
Reads the temperature from movisens files, resamples it and adds 
it to the matrix where accelerations are stored}
\usage{
g.readtemp_movisens(datafile, desiredtz = "", from = c(), to = c())	
}
\arguments{
  \item{datafile}{
  Full path to the data file movisens folder.
  }
  \item{desiredtz}{
  see \link{g.getmeta}
  }
  \item{from}{
  Origin point to derive the temperature from movisens files (automatically calculated by GGIR)
  }
  \item{to}{
  End point to derive the temperature from movisens files (automatically calculated by GGIR)
  }
}
\value{
Data matrix with the temperature values resampled at 64 Hz.
}
  
\examples{
\dontrun{
P = g.readtemp_movisens(datafile, desiredtz = "", from = c(), to = c())
}
}
