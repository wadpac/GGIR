# Labels detected sustained inactivity periods by g.part3 as either part of the Sleep Period Time window or not

Combines output from
[g.part3](https://wadpac.github.io/GGIR/reference/g.part3.md) and guider
information to estimate sleep variables. See vignette paragraph "Sleep
and full day time-use analysis in GGIR" for an elaborate descript of the
sleep detection.

## Usage

``` r
g.part4(datadir = c(), metadatadir = c(), f0 = f0, f1 = f1, params_sleep = c(), 
    params_metrics = c(),  params_cleaning = c(), params_output = c(),
                   params_general = c(), verbose = TRUE, ...)
```

## Arguments

- datadir:

  Directory where the accelerometer files are stored, e.g. "C:/mydata",
  or list of accelerometer filenames and directories, e.g.
  c("C:/mydata/myfile1.bin", "C:/mydata/myfile2.bin").

- metadatadir:

  Directory that holds a folder 'meta' and inside this a folder 'basic'
  which contains the milestone data produced by
  [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md). The
  folderstructure is normally created by
  [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md) and
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md) will recognise
  what the value of metadatadir is.

- f0:

  File index to start with (default = 1). Index refers to the filenames
  sorted in alphabetical order

- f1:

  File index to finish with (defaults to number of files available,
  i.e., f1 = 0)

- params_sleep:

  List of parameters used for sleep analysis (GGIR part 3, 4, and 5):
  see documentation
  [g.part3](https://wadpac.github.io/GGIR/reference/g.part3.md).

- params_metrics:

  List of parameters used for metrics extraction (GGIR part 1): see
  documentation
  [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md).

- params_cleaning:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_output:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_general:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- verbose:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- ...:

  To ensure compatibility with R scripts written for older GGIR
  versions, the user can also provide parameters listed in the params\_
  objects as direct argument.

## Value

The function does not produce values but generates an RData file in the
milestone subfolder ms4.out which incudes a dataframe named
`nightsummary`. This dataframe is used in g.report.part4 to create two
reports one per night and one per person. See package vignette paragraph
"Output part 4" for description of all the variables.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## References

- van Hees VT, Sabia S, et al. (2018) AEstimating sleep parameters using
  an accelerometer without sleep diary, Scientific Reports.

- van Hees VT, Sabia S, et al. (2015) A novel, open access method to
  assess sleep duration using a wrist-worn accelerometer, PLoS ONE.

## Examples

``` r
  if (FALSE) { # \dontrun{
    metadatadir = "C:/myfolder/meta" # assumes that there is a subfolder in
    # metadatadir named 'ms3.out' containing the output from g.part3
    g.part4(metadatadir=metadatadir)
  } # }
```
