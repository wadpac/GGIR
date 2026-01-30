# function to analyse and summarize pre-processed output from [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md)

Loads the output from
[g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md) and then
applies [g.impute](https://wadpac.github.io/GGIR/reference/g.impute.md)
and [g.analyse](https://wadpac.github.io/GGIR/reference/g.analyse.md),
after which the output is converted to .RData-format which will be used
by [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md) to generate
reports. The variables in these reports are the same variables as
described in
[g.analyse](https://wadpac.github.io/GGIR/reference/g.analyse.md).

## Usage

``` r
g.part2(datadir = c(), metadatadir = c(), f0 = c(), f1 = c(),
        myfun = c(), params_cleaning = c(), params_247 = c(),
        params_phyact = c(), params_output = c(), params_general = c(),
        verbose = TRUE, ...)
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

- myfun:

  External function object to be applied to raw data. See details
  [applyExtFunction](https://wadpac.github.io/GGIR/reference/applyExtFunction.md).

- params_cleaning:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_247:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_phyact:

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

The function provides no values, it only ensures that other functions
are called and that their output is stored in the folder structure as
created with
[g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md).

## Details

GGIR comes with many processing parameters, which have been thematically
grouped in parameter objects (R list). By running print(load_params())
you can see the default values of all the parameter objects. When g.part
2 is used via [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)
you have the option to specifiy a configuration file, which will
overrule the default parameter values. Further, as user you can set
parameter values as input argument to both g.part2 and
[GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md). Directly
specified argument overrule the configuration file and default values.

See the GGIR package vignette or the details section in
[GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md) for a more
elaborate overview of parameter objects and their usage across GGIR.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## References

- van Hees VT, Gorzelniak L, Dean Leon EC, Eder M, Pias M, et al. (2013)
  Separating Movement and Gravity Components in an Acceleration Signal
  and Implications for the Assessment of Human Daily Physical Activity.
  PLoS ONE 8(4): e61691. doi:10.1371/journal.pone.0061691

- van Hees VT, Fang Z, Langford J, Assah F, Mohammad A, da Silva IC,
  Trenell MI, White T, Wareham NJ, Brage S. Auto-calibration of
  accelerometer data for free-living physical activity assessment using
  local gravity and temperature: an evaluation on four continents. J
  Appl Physiol (1985). 2014 Aug 7

## Examples

``` r
  if (FALSE) { # \dontrun{
    metadatadir = "C:/myresults/output_mystudy"
    g.part2(metadatadir)
  } # }
```
