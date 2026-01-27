# Perform temporal pattern analyses

This function aims to facilitate time-pattern analysis building on the
labelled time series derived in GGIR part 5

## Usage

``` r
g.part6(datadir = c(), metadatadir = c(), f0 = c(), f1 = c(),
                   params_general = c(), params_phyact = c(), params_247 = c(),
                   params_cleaning = c(), verbose = TRUE, ...)
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

- params_general:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_phyact:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_247:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_cleaning:

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
milestone subfolder ms6.out which incudes ... (TO BE COMPLETED). This
dataframe is used in g.report.part6 to create reports. See package
vignette paragraph (TO BE COMPLETED) for description of all the
variables.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## Examples

``` r
  if (FALSE) { # \dontrun{
    metadatadir = "C:/myfolder/meta"
    g.part6(metadatadir=metadatadir)
  } # }
```
