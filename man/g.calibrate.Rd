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
  g.calibrate(datafile, params_rawdata = c(), params_general = c(),
              params_cleaning = c(), ...)
}
\arguments{
  \item{datafile}{
    Name of accelerometer file
  }
  \item{params_rawdata}{
    See \link{g.part1}
  }
  \item{params_general}{
    See \link{g.part1}
  }
  \item{params_cleaning}{
    See \link{g.part1}
  }
  \item{...}{
     Any argument used in the previous version of g.calibrate, which will now
     be used to overrule the arguments specified with the parameter objects.
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
    autocalibration. Only applies to data where temperate data is collected
    and available to GGIR, such as GENEActiv, Axivity, and in some instances
    ad-hoc .csv data.
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
  Vincent T van Hees <v.vanhees@accelting.com>
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