\name{g.readtemp_movisens}
\alias{g.readtemp_movisens}
\title{
Reads the temperature from movisens files.
}
\description{
Reads the temperature from movisens files, resamples it and adds 
it to the matrix where accelerations are stored.
\usage{
g.readtemp_movisens(datafile)	
}
\arguments{
  \item{fnames}{
Full path to the directory where the movisens recording files are stored.
}
}
\value{
Data matrix with the temperature values resampled at 64 Hz.
}
  
\examples{
\dontrun{
P = g.readtemp_movisens(datafile)
}
}
