# convert external Epoch data to GGIR part 1 milestone data format

convert externally created Epoch data to GGIR part 1 milestone data
format. Function not intended for direct use by user. The aim is to
allow for using GGIR on top of extrnally derived epoch data. See
argument dataFormat in the
[GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md) for details on
how to use this functionality.

## Usage

``` r
convertEpochData(datadir = c(), metadatadir = c(),
                    params_general = c(), f0 = c(), f1 = c(), verbose = TRUE)
```

## Arguments

- datadir:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- metadatadir:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- params_general:

  Parameters object see
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- f0:

  File index to start with (default = 1). Index refers to the filenames
  sorted in alphabetical order

- f1:

  File index to finish with (defaults to number of files available,
  i.e., f1 = 0)

- verbose:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
