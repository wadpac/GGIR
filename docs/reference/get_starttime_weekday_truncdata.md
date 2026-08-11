# Get starttime (adjusted), weekday, and adjust data accordingly.

Function not intended for direct use by user. Used inside
[g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md) as an
intermediate step between loading the raw data and calibrating it. This
step includes extracting the starttime and adjusting it to nearest
integer number of long epoch window lengths in an hour, truncating the
data accordingly, and extracting the corresponding weekday.

## Usage

``` r
get_starttime_weekday_truncdata(monc, 
  dformat, data, header, desiredtz, sf,
  datafile, ws2, configtz = NULL)
```

## Arguments

- monc:

  See
  [g.inspectfile](https://wadpac.github.io/GGIR/reference/g.inspectfile.md)

- dformat:

  See
  [g.dotorcomma](https://wadpac.github.io/GGIR/reference/g.dotorcomma.md)

- data:

  Data part of
  [g.readaccfile](https://wadpac.github.io/GGIR/reference/g.readaccfile.md)
  output

- header:

  Header part of
  [g.readaccfile](https://wadpac.github.io/GGIR/reference/g.readaccfile.md)
  output

- desiredtz:

  See [g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md)

- sf:

  Numeric, sample frequency in Hertz

- datafile:

  See [g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md)

- ws2:

  Long epoch length

- configtz:

  See [g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md)

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
