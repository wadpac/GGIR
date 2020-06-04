\name{read.myacc.csv}
\alias{read.myacc.csv}
\title{
  Read custom csv files with accelerometer data
}
\description{
  Loads csv files with accelerometer data and standardises 
  the output format (incl. unit of measurement, timestamp format,
  header format, and column locations) to make the data compatible
  with other GGIR functions.
}
\usage{
  read.myacc.csv(rmc.file=c(), rmc.nrow=c(), rmc.skip = c(), rmc.dec=".",
                          rmc.firstrow.acc = 1, rmc.firstrow.header=c(),
                          rmc.header.length = c(),
                          rmc.col.acc = 1:3, rmc.col.temp = c(), 
                          rmc.col.time=c(),
                          rmc.unit.acc = "g", rmc.unit.temp = "C", 
                          rmc.unit.time = "POSIX",
                          rmc.format.time = "\%Y-\%m-\%d \%H:\%M:\%OS",
                          rmc.bitrate = c(), rmc.dynamic_range = c(), 
                          rmc.unsignedbit = TRUE,
                          rmc.origin = "1970-01-01",
                          rmc.desiredtz = "Europe/London",
                          rmc.sf = c(),
                          rmc.headername.sf = c(),
                          rmc.headername.sn = c(),
                          rmc.headername.recordingid = c(),
                          rmc.header.structure = c(),
                          rmc.check4timegaps = FALSE,
                          rmc.col.wear = c(),
                          rmc.doresample=FALSE)	
}
\arguments{
  \item{rmc.file}{
    Filename of file to be read.
  }
  \item{rmc.nrow}{
    Number of rows to read, same as nrow argument in read.csv and in fread.
  }
  \item{rmc.skip}{
    Number of rows to skip, same as skip argument in read.csv and in fread.
  }
  \item{rmc.dec}{
    Decimal used for numbers, same as skip argument in read.csv and in fread.
  }
  \item{rmc.firstrow.acc}{
    First row (number) of the acceleration data.
  }
  \item{rmc.firstrow.header}{
    First row (number) of the header. Leave blank if the file does not have
    a header.
  }
  \item{rmc.header.length}{
    If file has header, specify header length (numeric).
  }
  \item{rmc.col.acc}{
    Vector with three column (numbers) in which the acceleration signals
    are stored
  }
  \item{rmc.col.temp}{
    Scalar with column (number) in which the temperature is stored.
    Leave in default setting if no temperature is avaible. The temperature
    will be used by g.calibrate.
  }
  \item{rmc.col.time}{
    Scalar with column (number) in which the timestamps are stored.
    Leave in default setting if timestamps are not stored. 
  }
  \item{rmc.unit.acc}{
    Character with unit of acceleration values: "g", "mg", or "bit"
  }
  \item{rmc.unit.temp}{
    Character with unit of temperature values: (K)elvin, (C)elsius, or (F)ahrenheit
  }
  \item{rmc.unit.time}{
    Character with unit of timestamps: "POSIX",
    "UNIXsec" (seconds since origin, see argument origin), "character", or
    "ActivPAL" (exotic timestamp format only used in the ActivPAL
    activity monitor).
    
  }
  \item{rmc.format.time}{
    Format of timestamp, only used for rmc.unit.time: character and POSIX.
  }
  \item{rmc.bitrate}{
    Numeric: If unit of acceleration is a bit then provide bit rate, e.g. 12 bit.
  }
  \item{rmc.dynamic_range}{
    Numeric, if unit of acceleration is a bit then provide dynamic range deviation
    in g from zero, e.g. +/-6g would mean this argument needs to be 6. If you give this
    argument a character value the code will search the file header for elements with
    a name equal to the character value and use the corresponding numeric value
    next to it as dynamic range.
  }
  \item{rmc.unsignedbit}{
    Boolean, if unsignedbit = TRUE means that bits are only positive numbers.
   if unsignedbit = FALSE then bits are both positive and negative.
  }
  \item{rmc.origin}{
    Origin of time when unit of time is UNIXsec, e.g. 1970-1-1
  }
  \item{rmc.desiredtz}{
    Timezone in which device was configured and expriments took place.
    If experiments took place in a different timezone, then use this
    argument for the timezone in whcih the experiments took place and 
    argument configtz to specify where the device was configured (not implemented yet).
  }
  \item{rmc.sf}{
    Sample rate in Hertz, if this is stored in the file header then the that will be used
    instead.
  }
  \item{rmc.headername.sf}{
    If file has a header: Row name (character) under which the sample
    frequency can be found.
  }
  \item{rmc.headername.sn}{
    If file has a header: Row name (character) under which the
    serial number can be found.
  }
  \item{rmc.headername.recordingid}{
    If file has a header: Row name (character) under which the
    recording ID can be found.
  }
  
  \item{rmc.header.structure}{
    Character used to split the header name from the header
    value, e.g. ":" or " "
  }
  \item{rmc.check4timegaps}{
    Boolean to indicate whether gaps in time should be imputed with zeros.
    Some sensing equipment provides accelerometer with gaps in time. The rest of 
    GGIR is not designed for this, by setting this argument to TRUE the the gaps
    in time will be filled with zeros.
  }
  \item{rmc.col.wear}{
    If external wear detection outcome is stored as part of the data then this can be used by GGIR.
    This argument specifies the column in which the wear detection (Boolean) is stored.
  }
  \item{rmc.doresample}{
    Boolean to indicate whether to resample the data based on the available timestamps and extracted 
   sample rate from the file header
  }

}
\value{
  List with objects data holding the time series of acceleration, and
  header if it was available in the orignal file.
}
\examples{
  # create test files: No header, with temperature, with time
  N = 30
  sf = 30
  timestamps = as.POSIXlt(Sys.time()+((0:(N-1))/sf),origin="1970-1-1",tz = "Europe/London")
  mydata = data.frame(x=rnorm(N), time=timestamps,y=rnorm(N),z=rnorm(N),temp=rnorm(N)+20)
  testfile = "testcsv1.csv"
  write.csv(mydata, file= testfile, row.names = FALSE)
  loadedData = read.myacc.csv(rmc.file=testfile, rmc.nrow=20, rmc.dec=".",
                      rmc.firstrow.acc = 1, rmc.firstrow.header=c(),
                      rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time=2,
                      rmc.unit.acc = "g", rmc.unit.temp = "C", rmc.origin = "1970-01-01")
  if (file.exists(testfile)) file.remove(testfile)
}
\author{
  Vincent T van Hees <vincentvanhees@gmail.com>
}