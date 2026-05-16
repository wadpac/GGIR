# Write flat epoch-level Parquet output

Builds a flat epoch-level table from `meta/ms5.outraw` time-series data
and writes it as a Parquet file. This is a legacy/compatibility export
pathway; the preferred dashboard export is
[`write_dashboard_parquet`](https://wadpac.github.io/GGIR/reference/write_dashboard_parquet.md).

## Usage

``` r
write_epoch_parquet(
  metadatadir = c(),
  params_output = c(),
  params_general = c(),
  params_phyact = c(),
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

- params_output:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_general:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_phyact:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- verbose:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

## Details

This function uses
[`build_epoch_table`](https://wadpac.github.io/GGIR/reference/build_epoch_table.md)
to read and harmonize epoch-level data (one row per epoch), then applies
type coercions and writes `results/parquet/ggir_epochs.parquet`.

Metadata stored in the Parquet footer includes export type, creation
timestamp, selected ms5 configuration name, acceleration metric, and
estimated epoch length in seconds.

This export path is mainly useful for workflows that prefer a separate
flat epoch table rather than nested epochs inside the dashboard parquet.

## Value

Invisibly returns the written Parquet file path, or `NULL` (invisibly)
when export is skipped.

## See also

[`GGIR`](https://wadpac.github.io/GGIR/reference/GGIR.md),
[`build_epoch_table`](https://wadpac.github.io/GGIR/reference/build_epoch_table.md),
[`write_dashboard_parquet`](https://wadpac.github.io/GGIR/reference/write_dashboard_parquet.md)

## Author

Samuel Timileyin Afolabi \<samuelafolabimails@gmail.com\>

## Examples

``` r
if (FALSE) { # \dontrun{
write_epoch_parquet(
  metadatadir = "path/to/output_run/output_test_run",
  params_output = list(),
  params_general = list(desiredtz = "", acc.metric = "ENMO"),
  params_phyact = list(part6_threshold_combi = "40_100_400"),
  verbose = TRUE
)
} # }
```
