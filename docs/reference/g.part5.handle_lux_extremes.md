# Check lux values for extremes and imputes or removes them

Extreme values are imputed by mean of neightbours if they occur isolated
or in a sequence of two, and removed if they occure in a sequence of 3
or longer.

## Usage

``` r
g.part5.handle_lux_extremes(lux)
```

## Arguments

- lux:

  Vector with lux values

## Value

List of imputed lux values and a vector with matching length named
correction_log indicating which timestamps where imputed (value=1),
replaced by NA (value=2) or untouched (value=0).

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
