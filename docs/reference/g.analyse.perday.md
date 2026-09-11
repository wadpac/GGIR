# Function supports [g.analyse](https://wadpac.github.io/GGIR/reference/g.analyse.md). Not intended for direct use by user.

Generates day specific analyses and fills corresponding output matrix,
[g.analyse](https://wadpac.github.io/GGIR/reference/g.analyse.md).

## Usage

``` r
g.analyse.perday(ndays, firstmidnighti, time, nfeatures, 
                midnightsi, metashort, averageday,
                doiglevels, nfulldays,lastmidnight, ws3, ws2, qcheck,
                fname, sensor.location, wdayname, tooshort, includedaycrit,
                doquan, quantiletype, doilevels, domvpa,
                mvpanames, wdaycode, ID,
                deviceSerialNumber, ExtFunColsi, myfun,
                params_247 = c(), params_phyact = c(),
                params_general = c(),
                ...)
```

## Arguments

- ndays:

  Number of days in file

- firstmidnighti:

  see
  [g.detecmidnight](https://wadpac.github.io/GGIR/reference/g.detecmidnight.md)

- time:

  timestamp column from metalong converted to character

- nfeatures:

  estimate of number of variables that need to be stored in the output
  matrix

- midnightsi:

  see
  [g.detecmidnight](https://wadpac.github.io/GGIR/reference/g.detecmidnight.md)

- metashort:

  see [g.impute](https://wadpac.github.io/GGIR/reference/g.impute.md)

- averageday:

  As produced by
  [g.impute](https://wadpac.github.io/GGIR/reference/g.impute.md)

- doiglevels:

  Boolean to indicate whether iglevels should be calculated

- nfulldays:

  Number of days between the first and last midnight in the recording

- lastmidnight:

  see
  [g.detecmidnight](https://wadpac.github.io/GGIR/reference/g.detecmidnight.md)

- ws3:

  Epoch size in seconds

- ws2:

  see [g.weardec](https://wadpac.github.io/GGIR/reference/g.weardec.md)

- qcheck:

  vector with zeros and ones for each epoch, respenting the quality
  check derived with g.impute

- fname:

  RData filename produced by g.part1

- sensor.location:

  as produced by
  [g.extractheadervars](https://wadpac.github.io/GGIR/reference/g.extractheadervars.md)

- wdayname:

  character with weekdayname

- tooshort:

  0 (file not too short) or 1 (file too short)

- includedaycrit:

  see [g.analyse](https://wadpac.github.io/GGIR/reference/g.analyse.md)

- doquan:

  Boolean whether quantile analysis should be done

- quantiletype:

  see [g.analyse](https://wadpac.github.io/GGIR/reference/g.analyse.md)

- doilevels:

  Boolean whether to generate ilevels, see
  [g.analyse](https://wadpac.github.io/GGIR/reference/g.analyse.md)

- domvpa:

  Boolean whether to do mvpa analysis

- mvpanames:

  Matrix with 6 columns and 1 row holding the names for the six mvpa
  variables

- wdaycode:

  Equal to M\$wday as produced by
  [g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md)

- ID:

  Person Identification number, this can be numeric or character

- deviceSerialNumber:

  As produced by
  [g.extractheadervars](https://wadpac.github.io/GGIR/reference/g.extractheadervars.md)

- ExtFunColsi:

  column index of metashort where metric is stored

- myfun:

  External function object to be applied to raw data, see
  [g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md).

- params_247:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- params_phyact:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- params_general:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- ...:

  Any argument used in the previous version of g.analyse.perday, which
  will now be used to overrule the arguments specified with the
  parameter objects.

## Value

- `daysummary`:

  Summary per day for the file that was analysed

- `ds_names`:

  Variable names in daysummary

- `windowsummary`:

  Window summary, only used when selectdayfile is specified

- `ws_names`:

  Variable names in windowsummary

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
