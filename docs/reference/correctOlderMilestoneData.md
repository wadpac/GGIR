# Corrects milestone data from g.part1 generated in older GGIR versions

Some older versions of GGIR stored the milestone data in part 1 as
factor. This function identifies when that occurs and convert the
affected columns to the appropriate class (e.g., numeric).

## Usage

``` r
correctOlderMilestoneData(x)
```

## Arguments

- x:

  Data frame with metashort or metalong data as generated in
  [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md)

## Value

Data frame with the class fixed in the appropriate columns (i.e., light
and temperature columns)

## Examples

``` r
  if (FALSE) { # \dontrun{
    correctOlderMilestoneData(x)
  } # }
```
