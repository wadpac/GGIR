# part6AlignIndividuals

Align individual time series per household where households are
identified by the character or number string between the first and
second '-' in the filename.

## Usage

``` r
part6AlignIndividuals(GGIR_ts_dir = NULL, outputdir = NULL,
                      path_ggirms = NULL, desiredtz = "", verbose = TRUE)
```

## Arguments

- GGIR_ts_dir:

  Character, path to time series directory in the GGIR output

- outputdir:

  Directory where you would like to store the output

- path_ggirms:

  path to GGIR created folder named meta, with the milestone data files

- desiredtz:

  Character, specifying the timezone database name of the timezone the
  data was collected in.

- verbose:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

## Value

no object is returned, only files are created in the output directory
