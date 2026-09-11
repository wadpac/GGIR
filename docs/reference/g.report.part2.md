# Generate report from milestone data produced by [g.part2](https://wadpac.github.io/GGIR/reference/g.part2.md)

Creates report from milestone data produced by
[g.part2](https://wadpac.github.io/GGIR/reference/g.part2.md). Not
intended for direct use by package user

## Usage

``` r
g.report.part2(metadatadir = c(), f0 = c(), f1 = c(),
                  store.long = FALSE, params_output, myfun = c(), 
                  verbose = TRUE, desiredtz = "")
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

- store.long:

  Booelean to indicate whether output should stored in long format in
  addition to default wide format. Automatically turned to TRUE if using
  day segmentation with qwindow.

- params_output:

  Parameters object, see
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- myfun:

  (Optional) List as documented in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).
  g.report.part2 uses this object to extract the name of the external
  function being used, such that this can be reused in the output
  filenames.

- verbose:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- desiredtz:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

## Value

Function does not produce data, but only writes reports in csv format
and visual reports in pdf format

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
