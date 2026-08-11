# Check whether character timestamp is in iso8601 format.

Checks whether timestamp stored in character format is in ISO8601 format
or not

## Usage

``` r
is.ISO8601(x)
```

## Arguments

- x:

  Timestamps in character format either in ISO8601 or as "yyyy-mm-dd
  hh:mm:ss".

## Examples

``` r
x ="1980-1-1 18:00:00"
is.ISO8601(x)
#> [1] FALSE
```
