# Generate user-friendly visual report. The first part of the report summarizes important daily metrics in bar plot format. The second part of the report shows the raw data and annotations in 24-hr periods. Angle-z is shown with sleep annotations during the SPT (sleep period time) window. ENMO is shown with daytime inactivity and PA (physical activity) annotations in the lower section of each 24-hr plot. The PA annotations are based on a 10 minute bout metric and 80 of a 10 minute bout of MVPA. Vigorous PA is a short window of time above threshold.vig that is part of a bout of MVPA. Light PA is a short window of time above threshold.lig that is part of a bout of light PA.

Function called by
[GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md) to generate
report. Not intended for direct use by user

## Usage

``` r
g.plot5(metadatadir = c(), dofirstpage = FALSE, viewingwindow = 1,
  f0 = c(), f1 = c(), overwrite = FALSE, metric="ENMO",desiredtz = "",
  threshold.lig = 30, threshold.mod = 100, threshold.vig = 400, 
  visualreport_without_invalid = TRUE, includedaycrit = 0.66, includenightcrit = 0.66,
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

- dofirstpage:

  Boolean to indicate whether a first page with historgrams summarizing
  the whole measurement should be added

- viewingwindow:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- f0:

  File index to start with (default = 1). Index refers to the filenames
  sorted in alphabetical order

- f1:

  File index to finish with (defaults to number of files available,
  i.e., f1 = 0)

- overwrite:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- metric:

  Which one of the metrics do you want to consider to describe
  behaviour. The metric of interest need to be calculated in M (see
  [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md))

- desiredtz:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- threshold.lig:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- threshold.mod:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- threshold.vig:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- visualreport_without_invalid:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- includenightcrit:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- includedaycrit:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- verbose:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

## Value

No values, this function only generates a plot

## Author

Vincent T van Hees \<v.vanhees@accelting.com\> Matthew R Patterson
\<mpatterson@shimmersensing.com\>

## Examples

``` r
  if (FALSE) { # \dontrun{
    # generate plots for the first 10 files:
    g.plot5(metadatadir="C:/output_mystudy/meta/basic",dofirstpage=TRUE,
    viewingwindow = 1,f0=1,f1=10,overwrite=FALSE,desiredtz = "Europe/London",
    threshold.lig,threshold.mod,threshold.vig)
  } # }
```
