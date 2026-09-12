# Build a flat epoch table from ms5.outraw time series

Reads epoch-level files from `meta/ms5.outraw` and constructs a single
flat data frame with one row per epoch.

## Usage

``` r
build_epoch_table(
  metadatadir = c(),
  params_general = c(),
  params_phyact = c(),
  results_dir = NULL,
  verbose = TRUE
)
```

## Arguments

- metadatadir:

  Directory that holds a folder 'meta' and inside this a folder
  'ms5.outraw' which contains the milestone data produced by
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md). The
  folder structure is normally created by
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md) and
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md) will recognise
  what the value of metadatadir is.

- params_general:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_phyact:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- results_dir:

  Optional character scalar. Path to `results`. If `NULL`, it is derived
  from `metadatadir`.

- verbose:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

## Details

The function:

1.  Selects an ms5 configuration folder (prefer requested threshold
    combination if available).

2.  Reads time-series files (CSV preferred over RData).

3.  Infers missing `filename` and maps `filename -> ID` from
    `results/part2_summary.csv` when needed.

4.  Derives `calendar_date` and `weekday` from epoch `timenum` using
    timezone information.

5.  Harmonizes column sets across recordings by filling missing columns
    with `NA`.

## Value

Either `NULL` (if no usable epoch data were found), or a list with:

- `epoch_df`: data frame with one row per epoch.

- `config_name`: selected ms5 configuration folder name.

## See also

[`write_epoch_parquet`](https://wadpac.github.io/GGIR/reference/write_epoch_parquet.md),
[`write_dashboard_parquet`](https://wadpac.github.io/GGIR/reference/write_dashboard_parquet.md)

## Author

Samuel Timileyin Afolabi \<samuelafolabimails@gmail.com\>

## Examples

``` r
if (FALSE) { # \dontrun{
res <- build_epoch_table(
  metadatadir = "path/to/output_run/output_test_run",
  params_general = list(desiredtz = ""),
  params_phyact = list(part6_threshold_combi = "40_100_400"),
  verbose = TRUE
)

if (!is.null(res)) {
  names(res)
  nrow(res$epoch_df)
  res$config_name
}
} # }
```
