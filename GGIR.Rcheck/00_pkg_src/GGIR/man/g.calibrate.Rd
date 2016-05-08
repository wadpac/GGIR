\name{g.calibrate}
\alias{g.calibrate}
\title{
function to estimate calibration error and make recommendation for addressing it
}
\description{
Function starts by identifying ten second windows of non-movement. Next, the
average acceleration per axis per window is used to estimate calibration error 
(offset and scaling) per axis. The function provides recommended correction factors
to address the calibration error and a summary of the callibration procedure.
}
\usage{
g.calibrate(datafile, use.temp = TRUE, spherecrit = 0.3, minloadcrit = 72, 
printsummary = TRUE,chunksize=c(),windowsizes=c(5,900,3600),selectdaysfile=c(),
dayborder=0)
}
\arguments{
  \item{datafile}{
  name of accelerometer file
  }
  \item{use.temp}{
  use temperature sensor data if available (Geneactive only)
  }
  \item{spherecrit}{
  the minimum required acceleration value (in g) on both sides of 0 g
for each axis. Used to judge whether the sphere is sufficiently populated
  }
  \item{minloadcrit}{
  the minimum number of hours the code needs to read for the
autocalibration procedure to be effective (only sensitive to multitudes of 12 hrs, 
other values will be ceiled). After loading these hours only extra data is loaded 
if calibration error has not been reduced to under 0.01 g.
  }
  \item{printsummary}{
  if TRUE will print a summary when done
  }
  \item{chunksize}{number between 0.2 and 1 to specificy the size of chunks to be 
loaded as a fraction of a 12 hour period, e.g. 0.5 equals 6 hour chunks.
The default is 1 (12 hrs). For machines with less than 4Gb of RAM memory a value
below 1 is recommended.
}
 \item{windowsizes}{
  see \link{g.getmeta}
}
  \item{selectdaysfile}{
  see \link{g.part1}
  }
  \item{dayborder}{
  see \link{g.part1}
  }
}
\value{
 \item{\code{scale}}{scaling correction values, e.g. c(1,1,1) }
  \item{\code{offset}}{offset correction values, e.g. c(0,0,0)}
  \item{\code{tempoffset}}{correction values related to temperature, e.g. c(0,0,0)}
  \item{\code{cal.error.start}}{absolute difference between Euclidean norm during all
  non-movement windows and 1 g before autocalibration}
  \item{\code{cal.error.end}}{absolute difference between Euclidean norm during all 
  non-movement windows and 1 g after autocalibration}
  \item{\code{spheredata}}{average, standard deviation, Euclidean norm and temperature
  (if available) for all ten second non-movement windows as used for the
  autocalibration procedure}
  \item{\code{npoints}}{number of 10 second no-movement windows used to populate
  the sphere}
  \item{\code{nhoursused}}{number of hours of measurement data scanned to find
  the ten second time windows with no movement}
  \item{\code{meantempcal}}{mean temperature corresponding to the data as used for 
autocalibration. Only applies to data collected with GENEActiv monitor. 
  }
}
\examples{
\dontrun{
datafile = "C:/myfolder/testfile.bin"

#Apply autocalibration:
C = g.calibrate(datafile)
print(C$scale)
print(C$offset)
}
}
\author{
  Vincent T van Hees <vincentvanhees@gmail.com>
  Zhou Fang
}
\references{
\itemize{
\item van Hees VT, Fang Z, Langford J, Assah F, Mohammad A, da Silva IC, Trenell MI, 
White T, Wareham NJ, Brage S. Auto-calibration of accelerometer data for
free-living physical activity assessment using local gravity and temperature: 
an evaluation on four continents. J Appl Physiol (1985). 2014 Aug 7
}
}