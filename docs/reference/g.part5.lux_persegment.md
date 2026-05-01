# Extract key lux variables per segment of the data.

Extracts per segment of the day: mean lux, time above 1000 lux, time
awake, and time LUX imputed. Function not intended for direct use by
package user.

## Usage

``` r
g.part5.lux_persegment(ts, sse, LUX_day_segments, epochSize, desiredtz = "")
```

## Arguments

- ts:

  Data.frame with time series

- sse:

  Indices corresponding to the current time window (e.g. MM or WW)

- LUX_day_segments:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- epochSize:

  Numeric epoch size in seconds

- desiredtz:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

## Value

List with values (vector) of the derived variables and corresponding
names (vector).

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
