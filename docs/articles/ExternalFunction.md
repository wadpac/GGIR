# Embedding external functions in GGIR

**NOTE: If you are viewing this page via CRAN note that the main GGIR
documentation has been migrated to the [GGIR GitHub
pages](https://wadpac.github.io/GGIR/).**

## Introduction

If you like GGIR but want to use algorithms for raw data not included in
GGIR then the external function embedding feature can be the solution.
For example, you may want to pilot a new machine learned classifiction
algorithm but you do not want to write all the data cleaning and
aggregation steps needed for analysis of real life ‘out of the lab’
acceleormeter data.

**How it works:**

Internally GGIR loads the raw accelerometer data in memory blocks of
about 24 hours. When the data is in memory, corrected for calibration
error, and resampled to the sample rate required by your function, GGIR
applies its own default algorithms as well as the external function
provided by you (Python or R). The external function is expected to take
as input: A three-column matrix with the acceleration data corresponding
to the three acceleration axes, and an optional parameters argument
which can be of any R format (character, list, vector, data.frame, etc).
As output your external function is expected to produce a matrix or
data.frame with one or multiple columns corresponding to the output of
your external function.

## Example with external R function

In this example we will apply the function counts() from R package
[activityCounts](https://CRAN.R-project.org/package=activityCounts) to
the raw data, which produces an estimate of Actigraph counts per second.

### Write external function

Create file **calculateCounts.R** and insert the following code:

``` r

calculateCounts = function(data=c(), parameters=c()) {
  # data: 3 column matrix with acc data
  # parameters: the sample rate of data
  library("activityCounts")
  if (ncol(data) == 4) data = data[,2:4]
  mycounts = counts(data = data, hertz = parameters, 
                    x_axis = 1, y_axis = 2, z_axis = 3,
                    start_time = Sys.time())
  mycounts = mycounts[,2:4] #Note: do not provide timestamps to GGIR
  return(mycounts)
}
```

### Provide external function to GGIR

Create a new .R file for running the GGIR analysis, e.g. named
`myscript.R`, and insert the following code. Do not forget to update the
filepath on the first line to point to your `calculateCounts.R` file.

``` r

source("~/calculateCounts.R")
myfun =  list(FUN = calculateCounts,
              parameters = 30,
              expected_sample_rate = 30,
              expected_unit = "g",
              colnames = c("countsX","countsY","countsZ"),
              outputres = 1,
              minlength = 1,
              outputtype = "numeric",
              aggfunction = sum,
              timestamp = F,
              reporttype = c("scalar", "scalar", "scalar"))
```

The above code creates object `myfun` of type `list` which is expected
to come with the following elements:

- `FUN` A character string specifying the location of the external
  function you want to apply.
- `parameters` The parameters used by the function, which can be stored
  in any format (vector, matrix, list, data.frame). The user should make
  sure that the external function can handle this object.
- `expected_sample_rate` Expected sample rate, if the inputdata has a
  difference sample rate, then the data will be resampled.
- `expected_unit` Expected unit of the acceleration by external
  function: “mg”, “g” or “ms2”. If input data is different it will be
  converted.
- `colnames` Character vector with the names of the columns produced by
  the external function.
- `outputres` The resolution (seconds) of the output produced by the
  external function. Note, that this needs to be equal to or a multitude
  of the short epoch size of the g.part1 output (5 seconds) or the short
  epoch size should be a multitude of this resolution. In this way GGIR
  can aggregate or repeat the external function output to be used inside
  GGIR, see [Aggregation and missing
  values](#aggregation-and-missing-values).
- `minlength` The minimum length (seconds) of input data needed,
  typically the window per which output is provided.
- `outputtype` Character to indicate the type of external function
  output. Set to “numeric” if data is stored in numbers (any numeric
  format), or “character” if it is a character string. Provide either
  one value for all output columns or one value per column,
  e.g. `c("numeric", "character")`.
- `aggfunction` If the data needs to be aggregated to match the short
  epoch size of the g.part1 output (5 seconds) then this element
  specifies what function should be used for the aggregation, e.g. mean,
  sum, median. Provide either one function for all output columns or a
  list with one function per column. See [Aggregation and missing
  values](#aggregation-and-missing-values) for when it is required and
  how missing values are handled.
- `timestamp` Boolean to indicate whether timestamps (seconds since
  1-1-1970) should be passed to the external function as the first
  column of the data matrix. The external function may then return
  either only its output columns, or the timestamp as an additional
  first column.
- `reporttype` Character to indicate the type of reporting by GGIR:
  “scalar” if it should be averaged per day, “event” if it should be
  summed per day, or “type” if it is a categorical variable that should
  be tabulated (see [External functions with categorical
  output](https://wadpac.github.io/GGIR/articles/ExternalFunctionType.html)
  for an example). Provide either one value for all output columns or
  one value per column. For backward compatibility, GGIR treats outputs
  as `"scalar"` when `reporttype` is omitted. For
  `reporttype = "event"`, GGIR stores the summaries in
  part2_eventsummary.csv and part2_dayeventsummary.csv rather than
  part2_summary.csv and part2_daysummary.csv. When several event columns
  are returned, their column names are included in event summary
  variable names to keep the results distinct.
- `name` Character to indicate the name of the entity being detected in
  the cases of event detection, this name is used in the output
  filename, e.g. part2_stepsummary.csv when name is “step”. At the
  moment name is only use for event detection algorithms

Next, add a call to GGIR function GGIR with `myfun` provided as one of
its arguments:

``` r

library(GGIR)
GGIR(datadir = "~/myaccelerometerdata",
             outputdir = "~/myresults",
             mode = 1:2,
             epochvalues2csv = TRUE,
             do.report = 2,
             myfun = myfun) #<= this is where object myfun is provided to GGIR
```

Please see the
[](https://cran.r-project.org/package=GGIR/vignettes/GGIR.html) for more
information about function GGIR.

## Example with external Python function

In this example we will use an external Python function to estimate the
dominant signal frequency per acceleration axis. Note this can also be
done in R, but it shows that even Python functions can be provided.

### Write external function

Create **dominant_frequency.py** and insert the code shown below:

``` python
import numpy

def dominant_frequency(x, sf):
  # x: vector with data values
  # sf: sample frequency
  fourier = numpy.fft.fft(x)
  frequencies = numpy.fft.fftfreq(len(x), 1/sf)
  magnitudes = abs(fourier[numpy.where(frequencies > 0)])
  peak_frequency = frequencies[numpy.argmax(magnitudes)]
  return peak_frequency
```

Create **dominant_frequency.R** that calls the python function and
insert the following code:

``` r
dominant_frequency = function(data=c(), parameters=c()) {
  # data: 3 column matrix with acc data
  # parameters: the sample rate of data
  source_python("dominant_frequency.py")
  sf=parameters
  N = nrow(data)
  ws = 5 # windowsize
  if (ncol(data) == 4) data= data[,2:4]
  data = data.frame(t = floor(seq(0,(N - 1)/sf, by = 1/sf)/ws),
                    x = data[,1], y = data[,2], z = data[,3])
  df = aggregate(data, by = list(data$t), 
                 FUN=function(x) {return(dominant_frequency(x, sf))})
  df = df[, -c(1:2)]
  return(df)
}
}
```

### Provide external function to GGIR

Create a new .R file for running the GGIR analysis, e.g. named
myscript.R, and insert the following blocks of code.

Specification of Python environment to use, this can also be a conda
environment or docker container (see documentation R package
[](https://rstudio.github.io/reticulate/) for further details). Make
sure that that this Python environment has all the required dependencies
for the external function, here we will only need `numpy`.

``` r

  library("reticulate")
  use_virtualenv("~/myvenv", required = TRUE) # Local Python environment
  py_install("numpy", pip = TRUE)
```

Specify a `myfun` object as explained in the R example. Do not forget to
update the filepath to the `"~/dominant_frequency.R"` file.

``` r
source("~/dominant_frequency.R")
myfun =  list(FUN = dominant_frequency,
              parameters = 30,
              expected_sample_rate = 30,
              expected_unit = "g",
              colnames = c("domfreqX", "domfreqY", "domfreqZ"),
              minlength = 5,
              outputres = 5,
              outputtype = "numeric",
              aggfunction = median
              timestamp = F,
              reporttype = c("scalar", "scalar", "scalar"))
```

Add a call to function GGIR where `myfun` is provided as argument. Note
that, do.parallel is set to FALSE. Unfortunately Python embedding with R
package reticulate and multi-threading with R package foreach as used in
GGIR do not combine well.

``` r

library(GGIR)
GGIR(datadir = "~/myaccelerometerdata",
             outputdir = "~/myresults",
             mode = 1:2,
             epochvalues2csv = TRUE,
             do.report = 2,
             myfun = myfun,
             do.parallel = FALSE)
```

## Multiple output columns

An external function can return more than one output column, for example
as a `data.frame` or a matrix. Each column is described by the
corresponding value in `colnames`, which must be unique. Elements
`outputtype`, `reporttype` and `aggfunction` can be given either as a
single value that applies to all columns, or as one value per column in
the same order as `colnames`. This makes it possible to combine
different kinds of output in one function, for example a categorical
activity type together with a numeric intensity:

``` r

myfun$colnames = c("activity", "intensity")
myfun$outputtype = c("character", "numeric")
myfun$reporttype = c("type", "scalar")
myfun$aggfunction = list(function(x) x[1], mean)
```

Each output column is processed and reported independently according to
its own `outputtype` and `reporttype`.

## Aggregation and missing values

When `outputres` is shorter than the short epoch size of GGIR (first
value of argument `windowsizes`), GGIR aggregates the values of each
epoch with `aggfunction`, and `aggfunction` must be provided. When
`outputres` equals the epoch size the output is used as is, and when
`outputres` is longer GGIR repeats each value to fill the epochs it
covers; in both cases `aggfunction` is not needed.

When the output columns need different aggregation, for example when
numeric and character outputs are combined, provide `aggfunction` as a
list with one function per output column.

GGIR passes all values of each epoch to `aggfunction`, including missing
values (`NA`) produced by the external function. Epochs containing only
missing values are retained, preserving time alignment; their aggregated
value depends on `aggfunction`. For example,
`function(x) sum(x, na.rm = TRUE)` returns `0` for an all-missing epoch.
Missing values are handled as follows:

- `mean`, `sum` and similar functions return `NA` if any value in the
  epoch is missing. Use `function(x) mean(x, na.rm = TRUE)` to summarise
  the available values instead.
- `function(x) x[1]` returns `NA` when the first value is missing, even
  if later values in the epoch are not missing.
- For character outputs, a function that selects the most frequent value
  should convert to character before tabulating and return `NA` when all
  values are missing:

``` r

function(x) {
  counts = table(as.character(x))
  if (length(counts) == 0) NA_character_ else names(which.max(counts))
}
```

Character outputs reach `aggfunction` as factors, and
[`table()`](https://rdrr.io/r/base/table.html) on a factor also counts
levels that do not occur, so a plain
`names(sort(table(x), decreasing = TRUE))[1]` would assign the first
level to epochs in which all values are missing.

## Integration in GGIR output

### Part 1

The external function output is included in the time series produced by
function GGIR function g.part1 and stored in an RData-file in
`/output_nameofstudy/meta/basic`. The resolution of these output in GGIR
is set by GGIR argument `windowsizes`, which is `c(5,900,3600)` by
default. Here, the first element `5` specifies the short epoch size in
seconds. If the output of the external function is less then this
resolution it will be aggregated with the function as specificied by
aggfunction in the `myfun` object (see [Aggregation and missing
values](#aggregation-and-missing-values)). In the count example we used
the sum for this and for the dominant frequency example we used the
median.

### Part 2

Next, in part2 GGIR aims to detect non-wear periods and imputes those.
The impute time series can be found in the part 2 milestone data in
folder: `/output_nameofstudy/meta/ms2.out`. If you want these to be
directly stored in a csv file then set argument
`epochvalues2csv = TRUE`.

### Part 5

Part 5 summarises the time series per day, separately for the sustained
sleep period (`spt`) and the time outside the sustained sleep period
(`day`). At the moment only outputs with `reporttype = "type"` are
summarised in part 5. GGIR writes them to dedicated reports per output
column, named
`part5_extfuntype_<column>_daysummary_<window>_<sleep-definition>.csv`
and
`part5_extfuntype_<column>_personsummary_<window>_<sleep-definition>.csv`.
These contain the time, mean acceleration, and number of blocks and
bouts per category, separately for `spt` and `day`. See [External
functions with categorical
output](https://wadpac.github.io/GGIR/articles/ExternalFunctionType.html#part-5)
for a description of these variables.

Outputs with `reporttype = "scalar"` or `"event"` are not summarised in
part 5. The only exception is an event output named `step_count`, which
part 5 keeps in its time series (folder
`/output_nameofstudy/meta/ms5.outraw`, saved by default via
`save_ms5rawlevels = TRUE`) and in the visual report, but not in the
part 5 summary reports.

### Visual report

When `visualreport = TRUE`, the visual report that GGIR generates after
part 5 also shows external function output:

- `reporttype = "type"`: the output column is drawn as a coloured strip
  below the main plot, with one colour per category and the categories
  listed in the legend. When the time series is aggregated to a longer
  epoch (for example with `part5_agg2_60seconds = TRUE`), each epoch
  shows the category that took up most of its time. Epochs without a
  category (unclassified or invalid) are left blank.
- `reporttype = "event"`: the output column is drawn as vertical bars at
  the bottom of the plot, showing the number of events per minute. The
  bars are scaled to 120 per minute for an output named `step_count`,
  and to the highest rate in the recording otherwise. Note that part 5
  currently only keeps an event output named `step_count` in its time
  series, so event outputs with other names are not shown.
- `reporttype = "scalar"`: these outputs are not shown in the visual
  report.

If several output columns have the same `reporttype`, only the first of
them (in the order of `colnames`) is plotted. All output columns are
still included in the CSV reports.

## External functions released by GGIR collaborators:

- Wrist-based step detection algorithm:
  <https://github.com/ShimmerResearch/Verisense-Toolbox/tree/master/Verisense_step_algorithm>

which is not actively maintained, for the latest version of the function
see:
<https://github.com/wadpac/GGIR/blob/master/user-scripts/verisense_count_steps.R>

- Wrist-based sleep classification as described by Sundararajan et
  al. 2021 [link to
  paper](https://www.nature.com/articles/s41598-020-79217-x), and
  corresponding code is here:
  <https://github.com/wadpac/Sundararajan-SleepClassification-2021/tree/master/ggir_ext>
