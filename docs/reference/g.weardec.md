# Detects whether accelerometer is worn

Uses the object produced by
[g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md) to assess
whether the accelerometer was worn

## Usage

``` r
g.weardec(metalong, wearthreshold, ws2, params_cleaning = NULL,
            desiredtz = "", qwindowImp = c())
```

## Arguments

- metalong:

  Object produced by
  [g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md)

- wearthreshold:

  Number of axis that at least need to meet the non-wear criteria

- ws2:

  Large windowsize used in seconds to apply non-wear detection Small
  window size not needed, because this is inherent to the object M

- params_cleaning:

  Parameters object with cleaning paramete, see
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- desiredtz:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- qwindowImp:

  See [g.impute](https://wadpac.github.io/GGIR/reference/g.impute.md)

## Value

- `r1` Participant id extracted from file

- `r2` Night number

- `r3` Detected onset of sleep expressed as hours since the previous
  midnight

- `LC` fraction of 15 minute windows with more than 5 percent clipping

- `LC2` fraction of 15 minute windows with more than 80 percent clipping

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## Examples

``` r
  data(data.getmeta)
  params_cleaning = load_params()$params_cleaning
  params_cleaning[["nonwearFilterWindow"]] = c(22, 7)
  output = g.weardec(metalong = data.getmeta$metalong, wearthreshold = 2, ws2 = 900,  
                      params_cleaning = params_cleaning,
                      desiredtz = "", qwindowImp = NULL)
```
