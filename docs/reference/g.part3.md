# Detection of sustained inactivity periods as needed for sleep detection in g.part4.

Function called by function GGIR. It estimates the sustained inactivity
periods in each day, which are used as input for g.part4 which then
labels them as nocturnal sleep or day time sustained inactivity periods.
Typical users should work with function GGIR only.

## Usage

``` r
g.part3(metadatadir = c(), f0, f1, myfun = c(), 
  params_sleep = c(), params_metrics = c(), params_output = c(), 
  params_general = c(), verbose = TRUE,
  ...)
```

## Arguments

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

- myfun:

  External function object to be applied to raw data. See details
  [applyExtFunction](https://wadpac.github.io/GGIR/reference/applyExtFunction.md).

- params_sleep:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_metrics:

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

## Details

GGIR comes with many processing parameters, which have been thematically
grouped in parameter objects (R list). By running print(load_params())
you can see the default values of all the parameter objects. When g.part
3 is used via [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)
you have the option to specifiy a configuration file, which will
overrule the default parameter values. Further, as user you can set
parameter values as input argument to both g.part3 and
[GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md). Directly
specified argument overrule the configuration file and default values.

See the GGIR package vignette or the details section in
[GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md) for a more
elaborate overview of parameter objects and their usage across GGIR.

## Value

The function provides no values, it only ensures that other functions
are called and that their output is stored in .RData files.  

- `night` nightnumber

- `definition` definition of sustained inactivity. For example, T10A5
  refers to 10 minute window and a 5 degree angle (see paper for further
  explaination).

- `start.time.day` timestamp when the day started

- `nsib.periods` number of sustained inactivity bouts

- `tot.sib.dur.hrs` total duration of all sustained inactivity bouts

- `fraction.night.invalid` fraction of the night for which accelerometer
  data was invalid, e.g. monitor not worn

- `sib.period` number of sustained inactivity period

- `sib.onset.time` onset time of sustained inactivity period

- `sib.end.time` end time of sustained inactivity period

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## References

- van Hees VT, Sabia S, et al. (2015) A novel, open access method to
  assess sleep duration using a wrist-worn accelerometer, PLoS ONE,
  November 2015

- van Hees VT, Sabia S, et al. (2018) Estimating sleep parameters using
  an accelerometer without sleep diary. Scientific Reports.

## Examples

``` r
  if (FALSE) { # \dontrun{
    metadatadir = "C:/myfolder/meta" # assumes that there is a subfolder in
    # metadatadir named 'basic' containing the output from g.part1
    g.part3(metadatadir=metadatadir, anglethreshold=5,
    timethreshold=5, overwrite=FALSE)
  } # }
```
