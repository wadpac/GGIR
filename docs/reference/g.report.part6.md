# Generate report from milestone data produced by [g.part6](https://wadpac.github.io/GGIR/reference/g.part6.md)

Creates report from milestone data produced by
[g.part6](https://wadpac.github.io/GGIR/reference/g.part6.md). Not
intended for direct use by package user

## Usage

``` r
g.report.part6(metadatadir = c(), f0 = c(), f1 = c(),
                          params_cleaning = NULL, params_output, 
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

- params_cleaning:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_output:

  Parameters object, see
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- verbose:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

## Value

Function does not produce data, but only writes reports in csv format

The following files are stored in the root of the results folder:
part6_summary.csv

See package vignette "HouseHoldCoanalysis".

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
