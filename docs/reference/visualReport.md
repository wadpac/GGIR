# Generate visualisation of time series produced by part 5.

Function called by
[GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md). Not intended
for direct use by user

## Usage

``` r
visualReport(metadatadir = c(), 
              f0 = c(), f1 = c(), 
              verbose = TRUE,
              part6_threshold_combi = NULL, GGIRversion = NULL,
              params_sleep = NULL,
              params_output = NULL,
              params_general = NULL)
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

- verbose:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- part6_threshold_combi:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- GGIRversion:

  Character with GGIR version number

- params_sleep:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- params_output:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- params_general:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

## Value

No values, this function only generates a plot

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
