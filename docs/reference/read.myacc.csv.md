# Read custom csv files with accelerometer data

Loads csv files with accelerometer data and standardises the output
format (incl. unit of measurement, timestamp format, header format, and
column locations) to make the data compatible with other GGIR functions.

## Usage

``` r
read.myacc.csv(rmc.file=c(), rmc.nrow=Inf, rmc.skip = c(), rmc.dec=".",
                          rmc.firstrow.acc = c(), rmc.firstrow.header=c(),
                          rmc.header.length = c(),
                          rmc.col.acc = 1:3, rmc.col.temp = c(), 
                          rmc.col.time=c(),
                          rmc.unit.acc = "g", rmc.unit.temp = "C", 
                          rmc.unit.time = "POSIX",
                          rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                          rmc.bitrate = c(), rmc.dynamic_range = c(), 
                          rmc.unsignedbit = TRUE,
                          rmc.origin = "1970-01-01",
                          rmc.desiredtz = NULL,
                          rmc.configtz = NULL,
                          rmc.sf = c(),
                          rmc.headername.sf = c(),
                          rmc.headername.sn = c(),
                          rmc.headername.recordingid = c(),
                          rmc.header.structure = c(),
                          rmc.check4timegaps = FALSE,
                          rmc.col.wear = c(),
                          rmc.doresample = FALSE,
                          rmc.scalefactor.acc = 1,
                          interpolationType=1, 
                          PreviousLastValue = c(0, 0, 1),
                          PreviousLastTime = NULL,
                          desiredtz = NULL,
                          configtz = NULL,
                          header = NULL)
```

## Arguments

- rmc.file:

  Filename of file to be read if it is in the working directory, or full
  path to the file otherwise.

- rmc.nrow:

  Number of rows to read, same as nrow argument in
  [read.csv](https://rdrr.io/r/utils/read.table.html) and nrows in
  [fread](https://rdrr.io/pkg/data.table/man/fread.html). The whole file
  is read by default (i.e., rmc.nrow = Inf).

- rmc.skip:

  Number of rows to skip, same as skip argument in
  [read.csv](https://rdrr.io/r/utils/read.table.html) and in
  [fread](https://rdrr.io/pkg/data.table/man/fread.html).

- rmc.dec:

  Decimal used for numbers, same as skip argument in
  [read.csv](https://rdrr.io/r/utils/read.table.html) and in
  [fread](https://rdrr.io/pkg/data.table/man/fread.html).

- rmc.firstrow.acc:

  First row (number) of the acceleration data.

- rmc.firstrow.header:

  First row (number) of the header. Leave blank if the file does not
  have a header.

- rmc.header.length:

  If file has header, specify header length (numeric).

- rmc.col.acc:

  Vector with three column (numbers) in which the acceleration signals
  are stored

- rmc.col.temp:

  Scalar with column (number) in which the temperature is stored. Leave
  in default setting if no temperature is avaible. The temperature will
  be used by
  [g.calibrate](https://wadpac.github.io/GGIR/reference/g.calibrate.md).

- rmc.col.time:

  Scalar with column (number) in which the timestamps are stored. Leave
  in default setting if timestamps are not stored.

- rmc.unit.acc:

  Character with unit of acceleration values: "g", "mg", or "bit"

- rmc.unit.temp:

  Character with unit of temperature values: (K)elvin, (C)elsius, or
  (F)ahrenheit

- rmc.unit.time:

  Character with unit of timestamps: "POSIX", "UNIXsec" (seconds since
  origin, see argument rmc.origin), "character", or "ActivPAL" (exotic
  timestamp format only used in the ActivPAL activity monitor).

- rmc.format.time:

  Character string giving a date-time format as used by
  [strptime](https://rdrr.io/r/base/strptime.html). Only used for
  rmc.unit.time: character and POSIX.

- rmc.bitrate:

  Numeric: If unit of acceleration is a bit then provide bit rate, e.g.
  12 bit.

- rmc.dynamic_range:

  Numeric, if unit of acceleration is a bit then provide dynamic range
  deviation in g from zero, e.g. +/-6g would mean this argument needs to
  be 6. If you give this argument a character value the code will search
  the file header for elements with a name equal to the character value
  and use the corresponding numeric value next to it as dynamic range.

- rmc.unsignedbit:

  Boolean, if unsignedbit = TRUE means that bits are only positive
  numbers. if unsignedbit = FALSE then bits are both positive and
  negative.

- rmc.origin:

  Origin of time when unit of time is UNIXsec, e.g. 1970-1-1

- rmc.desiredtz:

  Deprecated, please see `desiredtz`.

- rmc.configtz:

  Deprecated, please see `configtz`.

- rmc.sf:

  Sample rate in Hertz, if this is stored in the file header then that
  will be used instead.

- rmc.headername.sf:

  If file has a header: Row name (character) under which the sample
  frequency can be found.

- rmc.headername.sn:

  If file has a header: Row name (character) under which the serial
  number can be found.

- rmc.headername.recordingid:

  If file has a header: Row name (character) under which the recording
  ID can be found.

- rmc.header.structure:

  Character used to split the header name from the header value, e.g.
  ":" or " "

- rmc.check4timegaps:

  Boolean to indicate whether gaps in time should be imputed with zeros.
  Some sensing equipment provides accelerometer with gaps in time. The
  rest of GGIR is not designed for this, by setting this argument to
  TRUE the the gaps in time will be filled with zeros.

- rmc.col.wear:

  If external wear detection outcome is stored as part of the data then
  this can be used by GGIR. This argument specifies the column in which
  the wear detection (Boolean) is stored.

- rmc.doresample:

  Boolean to indicate whether to resample the data based on the
  available timestamps and extracted sample rate from the file header

- rmc.scalefactor.acc:

  Numeric value (default 1) to scale the acceleration signals via
  multiplication. For example, if data is provided in m/s2 then by
  setting this to 1/9.81 we would derive gravitational units.

- interpolationType:

  Integer to indicate type of interpolation to be used when resampling
  time series (mainly relevant for Axivity sensors), 1=linear, 2=nearest
  neighbour.

- PreviousLastValue:

  Automatically identified last value in previous chunk of data read.

- PreviousLastTime:

  Automatically identified last timestamp in previous chunk of data
  read.

- desiredtz:

  Timezone in which device was worn.

- configtz:

  Timezone in which device was configured. If equal to desiredtz you can
  leave this in its default value.

- header:

  Header information that was extracted the previous time this file was
  read, to be re-used instead of being extracted again.

## Details

To use this function in the context of GGIR use all arguments from this
function, except rmc.file, rmc.nrow, and rmc.skip as input for function
[GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md) or
[g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md) and also
specify argument rmc.noise, which is not part of this function but
needed to tell GGIR what noise level to expect in the data. The
rmc.noise is taken from the params_rawdata object if not explicitly
specified by user.

## Value

List with objects data holding the time series of acceleration with
among others a column named "time" that holds the time expressed in
seconds since 1-1-1970, and header if a header was present in the input
file.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## Examples

``` r
  # create test files: No header, with temperature, with time
  N = 30
  sf = 30
  x = Sys.time()+((0:(N-1))/sf)
  timestamps = as.POSIXlt(x, origin="1970-1-1", tz = "Europe/London")
  mydata = data.frame(x = rnorm(N), time = timestamps, y = rnorm(N), z = rnorm(N),
            temp = rnorm(N) + 20)
  testfile = "testcsv1.csv"
  write.csv(mydata, file= testfile, row.names = FALSE)
  loadedData = read.myacc.csv(rmc.file=testfile, rmc.nrow=20, rmc.dec=".",
                      rmc.firstrow.acc = 1, rmc.firstrow.header=c(),
                      desiredtz = "",
                      rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time=2,
                      rmc.unit.acc = "g", rmc.unit.temp = "C", rmc.origin = "1970-01-01")
  if (file.exists(testfile)) file.remove(testfile)
#> [1] TRUE
  
```
