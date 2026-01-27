# Set monitor brand specific parameters

Set monitor brand specific thresholds for non-wear detection, clipping
etection, and blocksizes to be loaded. Not designed for direct use by
user.

## Usage

``` r
get_nw_clip_block_params(monc, dformat, deviceSerialNumber = "", sf,
                            params_rawdata)
```

## Arguments

- monc:

  See
  [g.inspectfile](https://wadpac.github.io/GGIR/reference/g.inspectfile.md)

- dformat:

  See
  [g.dotorcomma](https://wadpac.github.io/GGIR/reference/g.dotorcomma.md)

- deviceSerialNumber:

  As produced by
  [g.extractheadervars](https://wadpac.github.io/GGIR/reference/g.extractheadervars.md)

- sf:

  Numeric, sample frequency in Hertz

- params_rawdata:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
