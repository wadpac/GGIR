# Merge output from physical activity and sleep analysis into one report

Function to merge the output from
[g.part2](https://wadpac.github.io/GGIR/reference/g.part2.md) and
[g.part4](https://wadpac.github.io/GGIR/reference/g.part4.md) into one
report enhanced with profiling of sleep and physical activity stratified
across intensity levels and based on bouted periods as well as
non-bouted periods.

## Usage

``` r
g.part5(datadir = c(), metadatadir = c(), f0 = c(), f1 = c(),
                   params_sleep = c(), params_metrics = c(),
                   params_247 = c(), params_phyact = c(), 
                   params_cleaning = c(), params_output = c(),
                   params_general = c(), verbose = TRUE, ...)
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

- params_sleep:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_metrics:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_247:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_phyact:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_cleaning:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_output:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_general:

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
milestone subfolder ms5.out which incudes a dataframe named `output`.
This dataframe is used in g.report.part5 to create two reports one per
day and one per person. See package vignette paragraph "Output part 5"
for description of all the variables.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## Examples

``` r
  if (FALSE) { # \dontrun{
    metadatadir = "C:/myfolder/meta"
    g.part5(metadatadir=metadatadir)
  } # }
```
