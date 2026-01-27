# Generate report from milestone data produced by [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md)

Creates report from milestone data produced by
[g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md). Not
intended for direct use by package user

## Usage

``` r
g.report.part5(metadatadir = c(), f0 = c(), f1 = c(), loglocation = c(),
                          params_cleaning = NULL,
                          LUX_day_segments = c(), params_output,
                          verbose = TRUE)
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

- loglocation:

  see [g.part4](https://wadpac.github.io/GGIR/reference/g.part4.md)

- params_cleaning:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- LUX_day_segments:

  see [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md)

- params_output:

  Parameters object, see
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- verbose:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

## Value

Function does not produce data, but only writes reports in csv format

The following files are stored in the root of the results folder:
part5_daysummary\_\* part5_personsummary\_\*

The following files are stored in the folder results/QC:
part5_daysummary_full\_\*

See package vignette paragraph "Waking-waking or 24 hour time-use
analysis" and "Output part 5" for a more elaborative description of the
full day time-use and analysis and reporting.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
