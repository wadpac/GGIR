# Function to extract meta-data (features) from data in accelerometer file

Reads a accelerometer file in blocks, extracts various features and
stores average feature value per short or long epoch. Acceleration and
angle metrics are stored at short epoch length. The non-wear indication
score, the clipping score, temperature (if available), light (if
available), and Euclidean norm are stored at long epoch length. The
function has been designed and thoroughly tested with accelerometer
files from GENEA and GENEActiv bin files. Further, the function should
be able to cope with ActiGraph gt3x and csv files, Axivity cwa and csv
files, Movisens bin files, and ad-hoc csv files read through the
[read.myacc.csv](https://wadpac.github.io/GGIR/reference/read.myacc.csv.md)
function.

## Usage

``` r
g.getmeta(datafile, params_metrics = c(), params_rawdata = c(),
                     params_general = c(), params_cleaning = c(), daylimit = FALSE,
                     offset = c(0, 0, 0), scale = c(1, 1, 1), tempoffset = c(0, 0, 0),
                     meantempcal = c(), myfun = c(), inspectfileobject = c(), 
                     verbose = TRUE, ...)
```

## Arguments

- datafile:

  name of accelerometer file

- params_metrics:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_rawdata:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_general:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_cleaning:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- daylimit:

  number of days to limit (roughly), if set to FALSE no daylimit will be
  applied

- offset:

  offset correction value per axis, usage: value = scale(value,center =
  -offset, scale = 1/scale)

- scale:

  scaling correction value per axis, usage: value = scale(value,center =
  -offset, scale = 1/scale)

- tempoffset:

  temperature offset correction value per axis, usage: value =
  scale(value,center = -offset, scale = 1/scale) + scale(temperature,
  center = rep(averagetemperate,3), scale = 1/tempoffset)

- meantempcal:

  mean temperature corresponding to the data as used for
  autocalibration. If autocalibration is not done or if temperature was
  not available then leave blank (default)

- myfun:

  External function object to be applied to raw data. See details
  [applyExtFunction](https://wadpac.github.io/GGIR/reference/applyExtFunction.md).

- inspectfileobject:

  Output from the function
  [g.inspectfile](https://wadpac.github.io/GGIR/reference/g.inspectfile.md).

- verbose:

  Boolean (default = TRUE). to indicate whether console message should
  be printed. Note that warnings and error are always printed and can be
  suppressed with suppressWarning() or suppressMessages().

- ...:

  Any argument used in the previous version of g.getmeta, which will now
  be used to overrule the arguments specified with the parameter
  objects.

## Value

- metalong:

  dataframe with long epoch meta-data: EN, non-wear score, clipping
  score, temperature

- metashort:

  dataframe with short epoch meta-data: timestamp and metric

- tooshort:

  indicator of whether file was too short for processing (TRUE or FALSE)

- corrupt:

  indicator of whether file was considered corrupt (TRUE or FALSE)

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## References

- van Hees VT, Gorzelniak L, Dean Leon EC, Eder M, Pias M, et al. (2013)
  Separating Movement and Gravity Components in an Acceleration Signal
  and Implications for the Assessment of Human Daily Physical Activity.
  PLoS ONE 8(4): e61691. doi:10.1371/journal.pone.0061691

- Aittasalo M, Vaha-Ypya H, Vasankari T, Husu P, Jussila AM, and
  Sievanen H. Mean amplitude deviation calculated from raw acceleration
  data: a novel method for classifying the intensity of adolescents
  physical activity irrespective of accelerometer brand. BMC Sports
  Science, Medicine and Rehabilitation (2015).

## Examples

``` r
  if (FALSE) { # \dontrun{
    datafile = "C:/myfolder/testfile.bin"

    #Extract meta-data:
    M = g.getmeta(datafile)

    #Inspect first couple of rows of long epoch length meta data:
    print(M$metalong[1:5,])

    #Inspect first couple of rows of short epoch length meta data:
    print(M$metalong[1:5,])
  } # }
```
