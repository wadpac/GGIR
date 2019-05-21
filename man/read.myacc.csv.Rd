\name{read.myacc.csv}
\alias{read.myacc.csv}
\title{
  Read custom csv files with accelerometer data
}
\description{
  Loads csv files with accelerometer data and helps with standardising the format (incl. unit of measurement, timestamp format, header format, and column locations) to make the data compatible with other GGIR functions.
}
\usage{
  read.myacc.csv(file=c(), nrow=c(),  dec=".",
                          firstrow.acc = 1, firstrow.header=c(),
                          header.length = c(),
                          col.acc = 1:3, col.temp = c(), col.time=c(),
                          unit.acc = "g", unit.temp = "C", unit.time = "POSIX",
                          format.time = "\%Y-\%m-\%d \%H:\%M:\%OS",
                          bitrate = c(), dynamic_range = c(), unsignedbit = TRUE,
                          origin = "1970-01-01",
                          desiredtz = "Europe/London", samplefrequency = c(),
                          headername.samplefrequency = c(),
                          headername.deviceserialnumber = c(),
                          headername.recordingid = c(),
                          header.structure = c(),
                          check4timegaps = FALSE)	
}
\arguments{
  \item{file}{
    Filename
  }
  \item{nrow}{
    Number of rows to read
  }
  \item{dec}{
    Decimal used for numbers
  }
  \item{firstrow.acc}{
    First row (number) of the acceleration data.
  }
  \item{firstrow.header}{
    First row (number) of the header. Leave blank if the file does not have
    a header.
  }
  \item{header.length}{
    If file has header, specify header length (numeric).
  }
  \item{col.acc}{
    Vector with three column (numbers) in which the acceleration signals
    are stored
  }
  \item{col.temp}{
    Scalar with column (number) in which the temperature is stored.
    Leave in default setting if no temperature is avaible. The temperature
    will be used by g.calibrate.
  }
  \item{col.time}{
    Scalar with column (number) in which the timestamps are stored.
    Leave in default setting if timestamps are not stored. 
  }
  \item{unit.acc}{
    Character with unit of acceleration values: "g", "mg", or "bit"
  }
  \item{unit.temp}{
    Character with unit of temperature values: (K)elvin, (C)elsius, or (F)ahrenheit
  }
  \item{unit.time}{
    Character with unit of timestamps: "POSIX",
    "UNIXsec" (seconds since origin, see argument origin), or
    "ActivPAL" (exotic timestamp format only used in the ActivPAL
    activity monitor).
    
  }
  \item{format.time}{
    Format of timestamp (only needed when POSIX) is provided.
  }
  \item{bitrate}{
    Numeric: If unit of acceleration is a bit then provide bit rate, e.g. 12 bit.
  }
  \item{dynamic_range}{
    Numeric, if unit of acceleration is a bit then provide dynamic range deviation
    in g from zero, e.g. +/-6g would mean this argument needs to be 6.
  }
  \item{unsignedbit}{
    Boolean, if unsignedbit = TRUE means that bits are only positive numbers.
   if unsignedbit = FALSE then bits are both positive and negative.
  }
  \item{origin}{
    Origin of time when unit of time is UNIXsec, e.g. 1970-1-1
  }
  \item{desiredtz}{
    Timezone in which device was configured and expriments took place.
    If experiments took place in a different timezone, then use this
    argument for the timezone in whcih the experiments took place and 
    argument configtz to specify where the device was configured (not implemented yet).
  }
  \item{samplefrequency}{
    Sample rate in Hertz, if this is stored in the file header then the that will be used
    instead.
  }
  \item{headername.samplefrequency}{
    If file has a header: Row name (character) under which the sample
    frequency can be found.
  }
  \item{headername.deviceserialnumber}{
    If file has a header: Row name (character) under which the
    serial number can be found.
  }
  \item{headername.recordingid}{
    If file has a header: Row name (character) under which the
    recording ID can be found.
  }
  
  \item{header.structure}{
    Character used to split the header name from the header
    value, e.g. ":" or " "
  }
  \item{check4timegaps}{
    Boolean to indicate whether gaps in time should be imputed with zeros.
    Some sensing equipment provides accelerometer with gaps in time. The rest of 
    GGIR is not designed for this, by setting this argument to TRUE the the gaps
    in time will be filled with zeros.
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
  loadedData = read.myacc.csv(file=testfile, nrow=20, dec=".",
                      firstrow.acc = 1, firstrow.header=c(),
                      col.acc = c(1,3,4), col.temp = 5, col.time=2,
                      unit.acc = "g", unit.temp = "C", origin = "1970-01-01")
  if (file.exists(testfile)) file.remove(testfile)
}
\author{
  Vincent T van Hees <vincentvanhees@gmail.com>
}