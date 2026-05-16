# Write consolidated dashboard Parquet output

Creates a single dashboard-ready Parquet file by combining GGIR summary
outputs and attaching nested epoch-level time series by day.

## Usage

```r
write_dashboard_parquet(
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

The function reads already-generated CSV reports, starts from Part 5 day
summary as the base table, and left-joins additional fields from:

- Part 4 night summary

- Part 2 day summary

- Part 2 participant summary

- quality-control report

It then calls `build_epoch_lists_by_day` to construct a nested `epochs`
list-column and merges that into the day-level table by `ID` and
`calendar_date`.

Column names are cleaned for SQL-friendly use, common columns are
type-cast, and Parquet key-value metadata is embedded (variable
definitions, thresholds, behavioral-code mapping, acceleration metric,
and estimated epoch length).

Output filename is ID-based for single-participant exports and falls
back to a multi-participant name otherwise.

The generated Parquet file can be opened directly in the GGIR Web
Dashboard (<https://wadpac.github.io/ggir-web-dashboard/>), where
processing happens entirely in the browser.

## Value

Invisibly returns the written Parquet file path, or `NULL` (invisibly)
if export is skipped because required data are missing. The generated
Parquet file can be opened directly in the GGIR Web Dashboard:
<https://wadpac.github.io/ggir-web-dashboard/>

## See also

[`GGIR`](https://wadpac.github.io/GGIR/reference/GGIR.md),
[`write_epoch_parquet`](https://wadpac.github.io/GGIR/reference/write_epoch_parquet.md),
[`build_epoch_table`](https://wadpac.github.io/GGIR/reference/build_epoch_table.md)

## Author

Samuel Timileyin Afolabi \<samuelafolabimails@gmail.com\>

## Examples

```r
if (FALSE) { # \dontrun{
write_dashboard_parquet(
  metadatadir = "path/to/output_run/output_test_run",
  params_output = list(save_dashboard_parquet = TRUE),
  params_general = list(desiredtz = "", acc.metric = "ENMO"),
  params_phyact = list(part6_threshold_combi = "40_100_400"),
  verbose = TRUE
)
} # }
```
